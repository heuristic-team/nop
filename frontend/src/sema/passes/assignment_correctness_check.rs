use std::borrow::Cow;
use std::collections::HashSet;

use super::{Diagnostic, Pass, Res};
use crate::ast::*;
use crate::lexer::WithSpan;

pub struct AssignmentCorrectnessCheck {}

type MutableVars<'a> = HashSet<&'a str>;

fn is_expr_assignable(ctx: &MutableVars, expr: &Expr) -> bool {
    match expr {
        Expr::Num { .. } | Expr::Bool { .. } => false,
        Expr::Ref { name, .. } => ctx.contains(name.value.as_str()),

        Expr::Call { .. }
        | Expr::Block { .. }
        | Expr::Binary { .. }
        | Expr::Declare { .. }
        | Expr::Ret { .. } => false, // this may change when we add something like pointers
    }
}

fn check_expr<'a, 'b: 'a>(
    diags: &mut Vec<Diagnostic>,
    ctx: &'a mut Cow<MutableVars<'b>>,
    expr: &'b Expr,
) {
    match expr {
        Expr::Num { .. } | Expr::Ref { .. } | Expr::Bool { .. } => {}
        Expr::Call { callee, args, .. } => {
            check_expr(diags, ctx, callee);
            args.iter().for_each(|arg| check_expr(diags, ctx, arg));
        }
        Expr::Binary {
            op:
                WithSpan {
                    value: BinaryOp::Assign,
                    span: op_span,
                },
            lhs,
            rhs,
            ..
        } => {
            if !is_expr_assignable(&ctx, lhs) {
                diags.push(Diagnostic::new(
                    format!("left hand side is not assignable"),
                    *op_span,
                ))
            }
            check_expr(diags, ctx, lhs);
            check_expr(diags, ctx, rhs);
        }
        Expr::Binary { lhs, rhs, .. } => {
            check_expr(diags, ctx, lhs);
            check_expr(diags, ctx, rhs);
        }
        Expr::Declare {
            is_mut,
            name,
            value,
            ..
        } => {
            check_expr(diags, ctx, value);
            if *is_mut {
                ctx.to_mut().insert(&name.value);
            }
        }
        Expr::Block { body, .. } => {
            let mut ctx = Cow::Borrowed(ctx.as_ref());
            body.iter().for_each(|e| check_expr(diags, &mut ctx, e));
        }
        Expr::Ret { value, .. } => {
            if let Some(e) = value {
                check_expr(diags, ctx, e);
            }
        }
    }
}

fn check_decl(diags: &mut Vec<Diagnostic>, decl: &FnDecl) {
    let ctx = decl
        .params
        .iter()
        .filter(|p| p.is_mut)
        .map(|p| p.name.value.as_str())
        .collect::<MutableVars>();

    check_expr(diags, &mut Cow::Borrowed(&ctx), &decl.body)
}

impl Pass for AssignmentCorrectnessCheck {
    type Input = AST;
    type Output = AST;

    fn run(&mut self, ast: Self::Input) -> Res<Self::Output> {
        let mut diags = vec![];

        ast.values().for_each(|decl| check_decl(&mut diags, decl));

        if diags.is_empty() {
            Res::Ok(ast)
        } else {
            Res::Fatal(diags)
        }
    }
}
