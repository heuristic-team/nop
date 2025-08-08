use std::rc::Rc;

use super::{Pass, Res};
use crate::ast::*;
use crate::typesystem::{Type, TypeDecl};

/// Pass for replacing implicit returns with explicit ones in function bodies.
///
/// Generally, the algorithm is:
///
/// - if the return type is `unit`, the last expression (if it is not `ret`) should be ignored completely, so we just wrap it in a block and add a `ret` expression
/// - otherwise find the last expression in the body, and if it is not `ret`, replace it with a `ret` with that expression as the argument
pub struct HandleImplicitRets {}

impl Pass for HandleImplicitRets {
    type Input = (Vec<FnDecl>, Vec<TypeDecl>);
    type Output = (Vec<FnDecl>, Vec<TypeDecl>);

    fn run(&mut self, (mut fn_decls, type_decls): Self::Input) -> Res<Self::Output> {
        fn_decls.iter_mut().for_each(handle_implicit_rets_in_decl);
        Res::Ok((fn_decls, type_decls))
    }
}

fn handle_implicit_rets_in_decl(decl: &mut FnDecl) {
    let last_expr = find_implicit_ret_candidate(&mut decl.body);

    if matches!(last_expr, Expr::Ret { .. }) {
        return;
    }

    let span = last_expr.span();
    *last_expr = match *decl.return_type.value {
        Type::Unit => Expr::Block {
            tp: Rc::new(Type::Unit),
            span: last_expr.span(),
            body: vec![
                last_expr.clone(), // TODO: remove this clone :( pt.1
                Expr::Ret { value: None, span },
            ],
        },
        _ => {
            set_all_returned_conditionals_to_expr_pos(last_expr);
            Expr::Ret {
                value: Some(Box::new(last_expr.clone())), // TODO: remove this clone :( pt.2
                span,
            }
        }
    };
}

/// Find the returned conditional expressions and set full `if ... then ... else ...` chains among with their nested conditionals to the expression position so typecheck can handle them correctly.
fn set_all_returned_conditionals_to_expr_pos(e: &mut Expr) {
    match e {
        Expr::Declare { .. } | Expr::Ret { .. } => panic!(
            "expr {} is invalid when not in statement position, varify that parser is correct",
            stringify!(e)
        ),
        Expr::Block { body, .. } => {
            if let Some(last) = body.last_mut() {
                set_all_returned_conditionals_to_expr_pos(last);
            }
        }
        Expr::While { .. } => todo!(), // find `break`s and handle conditionals in them
        Expr::If {
            on_true,
            on_false,
            in_stmt_pos,
            ..
        } => {
            *in_stmt_pos = false;
            set_all_returned_conditionals_to_expr_pos(on_true);
            if let Some(on_false) = on_false {
                set_all_returned_conditionals_to_expr_pos(on_false);
            }
        }
        Expr::Num { .. } | Expr::Bool { .. } | Expr::Ref { .. } | Expr::Call { .. } => {}
        Expr::Binary { lhs, rhs, .. } => {
            set_all_returned_conditionals_to_expr_pos(lhs);
            set_all_returned_conditionals_to_expr_pos(rhs);
        }
    }
}

/// Find last expression in the expression tree, which may be a candidate for implicit return.
///
/// For blocks, it should be the candidate obtained from the last expression of the block, because blocks can be nested. For any other expression it is that expression itself.
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
