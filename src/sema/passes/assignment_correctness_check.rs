use std::collections::HashSet;

use super::Pass;
use super::Res;
use crate::ast::*;
use crate::lexer::WithSpan;
use crate::sema::Diagnostic;

pub struct AssignmentCorrectnessCheck {}

type MutableVars<'a> = HashSet<&'a str>;

fn is_expr_assignable(ctx: &MutableVars, expr: &Expr) -> bool {
    match expr {
        Expr::Num { .. } => false,
        Expr::Ref { name, .. } => ctx.contains(name.value.as_str()),
        Expr::Call { .. } => false,   // this will always be like that?
        Expr::Binary { .. } => false, // same question as above
    }
}

fn check_expr(ctx: &MutableVars, expr: &Expr) -> Vec<Diagnostic> {
    match expr {
        Expr::Num { .. } | Expr::Ref { .. } => vec![],
        Expr::Call { callee, args, .. } => {
            let mut res = check_expr(ctx, callee);
            for arg in args {
                res.append(&mut check_expr(ctx, arg));
            }
            res
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
            let mut res = vec![];
            if !is_expr_assignable(&ctx, lhs) {
                res.push(Diagnostic::new(
                    format!("left hand side is not assignable"),
                    *op_span,
                    vec![],
                ))
            }
            res.append(&mut check_expr(&ctx, lhs));
            res.append(&mut check_expr(&ctx, rhs));
            res
        }
        Expr::Binary { lhs, rhs, .. } => {
            let mut res = check_expr(&ctx, lhs);
            res.append(&mut check_expr(&ctx, rhs));
            res
        }
    }
}

fn check_decl(decl: &FnDecl) -> Vec<Diagnostic> {
    let mut ctx = decl
        .params
        .iter()
        .filter(|p| p.is_mut)
        .map(|p| p.name.value.as_str())
        .collect::<MutableVars>();

    let mut res = Vec::new();

    for stmt in &decl.body {
        let mut diags = match stmt {
            Stmt::Declare {
                is_mut,
                name,
                value,
                ..
            } => {
                let diags = check_expr(&ctx, value);
                if *is_mut {
                    ctx.insert(&name.value);
                }
                diags
            }
            Stmt::Expr(expr) => check_expr(&ctx, expr),
        };

        res.append(&mut diags);
    }

    res
}

impl Pass for AssignmentCorrectnessCheck {
    type Input = AST;
    type Output = AST;

    fn run(&mut self, ast: Self::Input) -> Res<Self::Output> {
        let mut diags = vec![];

        for decl in ast.values() {
            diags.append(&mut check_decl(decl));
        }

        if diags.is_empty() {
            Res::Ok(ast)
        } else {
            Res::Fatal(diags)
        }
    }
}
