// This module implements the Throws folder.
//
// The Throws folder actually visits the item being processed and performs two
// processes:
// - It ok wraps return expressions and inserts terminal Ok(())s.
// - It delegates return type rewriting to the Args type.

use proc_macro::*;
use syn::fold::Fold;
use syn::{ImplItemMethod, Stmt};

use crate::Args;

pub struct Throws {
    args: Args,
    outer_fn: bool,
}

impl Throws {
    pub fn new(args: Args) -> Throws {
        Throws { args, outer_fn: true }
    }

    pub fn fold(&mut self, input: TokenStream) -> TokenStream {
        if let Ok(item_fn) = syn::parse(input.clone()) {
            let item_fn = self.fold_item_fn(item_fn);
            return quote::quote!(#item_fn).into();
        } else if let Ok(method) = syn::parse::<ImplItemMethod>(input.clone()) {
            // syn will accept a fn definition with no body as ImplItemMethod, even if the
            // definition is in a trait. Attempt to detect this case and reject it.
            // NOTE: this is dependent on the internals of syn and how syn chooses to interpret
            // an empty method body.
            let mut has_body = true;
            if method.block.stmts.len() == 1 {
                if let Stmt::Item(_) = method.block.stmts[0] {
                    has_body = false;
                }
            }

            if has_body {
                let method = self.fold_impl_item_method(method);
                return quote::quote!(#method).into();
            }
        }

        if let Ok(method) = syn::parse(input.clone()) {
            let method = self.fold_trait_item_method(method);
            return quote::quote!(#method).into();
        } else {
            panic!("#[throws] attribute can only be applied to functions and methods")
        }
    }
}

impl Fold for Throws {
    fn fold_item_fn(&mut self, mut i: syn::ItemFn) -> syn::ItemFn {
        if !self.outer_fn { return i; }
        self.outer_fn = false;

        modify_tail(is_unit_fn(&i.sig.output), &mut i.block.stmts);

        let sig = syn::Signature {
            output: self.fold_return_type(i.sig.output),
            ..i.sig
        };


        let block = Box::new(self.fold_block(*i.block));

        syn::ItemFn { sig, block, ..i }
    }

    fn fold_impl_item_method(&mut self, mut i: syn::ImplItemMethod) -> syn::ImplItemMethod {
        if !self.outer_fn { return i; }
        self.outer_fn = false;

        modify_tail(is_unit_fn(&i.sig.output), &mut i.block.stmts);

        let sig = syn::Signature {
            output: self.fold_return_type(i.sig.output),
            ..i.sig
        };

        let block = self.fold_block(i.block);

        syn::ImplItemMethod { sig, block, ..i }
    }

    fn fold_trait_item_method(&mut self, mut i: syn::TraitItemMethod) -> syn::TraitItemMethod {
        if !self.outer_fn { return i; }
        self.outer_fn = false;

        let default = i.default.take().map(|mut block| {
            modify_tail(is_unit_fn(&i.sig.output), &mut block.stmts);
            self.fold_block(block)
        });

        let sig = syn::Signature {
            output: self.fold_return_type(i.sig.output),
            ..i.sig
        };

        syn::TraitItemMethod { sig, default, ..i }
    }

    fn fold_expr_closure(&mut self, i: syn::ExprClosure) -> syn::ExprClosure {
        i // TODO
    }

    fn fold_expr_async(&mut self, i: syn::ExprAsync) -> syn::ExprAsync {
        i // TODO
    }

    fn fold_return_type(&mut self, i: syn::ReturnType) -> syn::ReturnType {
        self.args.ret(i)
    }

    fn fold_expr_return(&mut self, i: syn::ExprReturn) -> syn::ExprReturn {
        let ok = match &i.expr {
            Some(expr)  => ok(expr),
            None        => ok_unit(),
        };
        syn::ExprReturn { expr: Some(Box::new(ok)), ..i }
    }
}

fn modify_tail(is_unit_fn: bool, stmts: &mut Vec<syn::Stmt>) {
    let last_non_item_stmt = stmts.iter_mut().rev().filter(|s| {
        if let syn::Stmt::Item(_) = s { false } else { true }
    }).next();
    match last_non_item_stmt {
        Some(syn::Stmt::Expr(e)) if is_unit_fn => {
            let new = syn::parse2(quote::quote!(#e;)).unwrap();
            stmts.pop();
            stmts.push(new);
            stmts.push(syn::Stmt::Expr(ok_unit()));
        }
        Some(syn::Stmt::Expr(e))    => {
            *e = ok(e);
        }
        _ if is_unit_fn             => {
            stmts.push(syn::Stmt::Expr(ok_unit()));
        }
        _                           => { }
    }
}


fn is_unit_fn(i: &syn::ReturnType) -> bool {
    match i {
        syn::ReturnType::Default        => true,
        syn::ReturnType::Type(_, ty)    => {
            if let syn::Type::Tuple(syn::TypeTuple { elems, .. }) = &**ty {
                elems.is_empty()
            } else { false }
        }
    }
}

fn ok(expr: &syn::Expr) -> syn::Expr {
    syn::parse2(quote::quote!(<_ as ::fehler::__internal::_Succeed>::from_ok(#expr))).unwrap()
}

fn ok_unit() -> syn::Expr {
    syn::parse2(quote::quote!(<_ as ::fehler::__internal::_Succeed>::from_ok(()))).unwrap()
}
