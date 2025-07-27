use std::rc::Rc;

use super::{Pass, Res};
use crate::ast::*;
use crate::typesystem::{Type, TypeDecl};

pub struct HandleImplicitRets {}

impl Pass for HandleImplicitRets {
    type Input = (Vec<FnDecl>, Vec<TypeDecl>);
    type Output = (Vec<FnDecl>, Vec<TypeDecl>);

    fn run(&mut self, (mut fn_decls, type_decls): Self::Input) -> Res<Self::Output> {
        fn_decls.iter_mut().for_each(process_decl);
        Res::Ok((fn_decls, type_decls))
    }
}

/// Replace implicit returns with explicit ones
fn process_decl(decl: &mut FnDecl) {
    let last_expr = find_implicit_ret_candidate(&mut decl.body);

    if matches!(last_expr, Expr::Ret { .. }) {
        return;
    }

    let span = last_expr.span();
    *last_expr = match *decl.tp.value {
        Type::Unit => Expr::Block {
            tp: Rc::new(Type::Unit),
            span: last_expr.span(),
            body: vec![
                last_expr.clone(), // TODO: remove this clone :( pt.1
                Expr::Ret { value: None, span },
            ],
        },
        _ => {
            set_all_conditionals_to_expr_pos(last_expr);
            Expr::Ret {
                value: Some(Box::new(last_expr.clone())), // TODO: remove this clone :( pt.2
                span,
            }
        }
    };
}

fn set_all_conditionals_to_expr_pos(e: &mut Expr) {
    match e {
        Expr::Declare { .. } | Expr::Ret { .. } => panic!("expr {} is invalid here", stringify!(e)),
        Expr::Block { body, .. } => {
            if let Some(last) = body.last_mut() {
                set_all_conditionals_to_expr_pos(last);
            }
        }
        Expr::While { .. } => todo!(), // find `break`s and handle them
        Expr::If {
            on_true,
            on_false,
            in_stmt_pos,
            ..
        } => {
            *in_stmt_pos = false;
            set_all_conditionals_to_expr_pos(on_true);
            if let Some(on_false) = on_false {
                set_all_conditionals_to_expr_pos(on_false);
            }
        }
        Expr::Num { .. } | Expr::Bool { .. } | Expr::Ref { .. } | Expr::Call { .. } => {}
        Expr::Binary { lhs, rhs, .. } => {
            set_all_conditionals_to_expr_pos(lhs);
            set_all_conditionals_to_expr_pos(rhs);
        }
    }
}

/// Find last expression in the expression tree, which may be a candidate for implicit return
fn find_implicit_ret_candidate(root: &mut Expr) -> &mut Expr {
    match root {
        Expr::Declare { .. }
        | Expr::Ret { .. }
        | Expr::Num { .. }
        | Expr::Bool { .. }
        | Expr::Ref { .. }
        | Expr::Call { .. }
        | Expr::Binary { .. }
        | Expr::While { .. }
        | Expr::If { .. } => root,
        Expr::Block { body, .. } if body.is_empty() => root,
        Expr::Block { body, .. } => body.last_mut().map(find_implicit_ret_candidate).unwrap(),
    }
}
