use super::{Pass, Res};
use crate::Diagnostic;
use crate::TranslationUnit;
use crate::ast::*;
use crate::lexer::WithSpan;
use crate::support::ScopedSet;

pub struct AssignmentCorrectnessCheck {}

type MutableVars<'a> = ScopedSet<&'a str>;

fn is_expr_assignable(ctx: &MutableVars, expr: &Expr) -> bool {
    match expr {
        Expr::Num { .. } | Expr::Bool { .. } | Expr::Ret { .. } | Expr::Declare { .. } => false,
        Expr::Ref { name, .. } => ctx.contains(name.value.as_str()),

        Expr::If { .. }
        | Expr::While { .. }
        | Expr::Call { .. }
        | Expr::Block { .. }
        | Expr::Binary { .. } => false, // this may change when we add something like pointers
    }
}

/// Recursively go through expression tree, find expressions like `dest = src` and
/// verify that their destination operand is an expression that is assignable to
fn find_and_check_assignments<'a, 'b: 'a>(
    diags: &mut Vec<Diagnostic>,
    ctx: &'a mut MutableVars<'b>,
    expr: &'b Expr,
) {
    match expr {
        Expr::Num { .. } | Expr::Ref { .. } | Expr::Bool { .. } => {}
        Expr::Call { callee, args, .. } => {
            find_and_check_assignments(diags, ctx, callee);
            args.iter()
                .for_each(|arg| find_and_check_assignments(diags, ctx, arg));
        }
        Expr::While { cond, body, .. } => {
            find_and_check_assignments(diags, ctx, cond);
            find_and_check_assignments(diags, ctx, body);
        }
        Expr::If {
            cond,
            on_true,
            on_false,
            ..
        } => {
            find_and_check_assignments(diags, ctx, cond);
            find_and_check_assignments(diags, ctx, on_true);
            if let Some(on_false) = on_false {
                find_and_check_assignments(diags, ctx, on_false);
            }
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
            find_and_check_assignments(diags, ctx, lhs);
            find_and_check_assignments(diags, ctx, rhs);
        }
        Expr::Binary { lhs, rhs, .. } => {
            find_and_check_assignments(diags, ctx, lhs);
            find_and_check_assignments(diags, ctx, rhs);
        }
        Expr::Declare {
            is_mut,
            name,
            value,
            ..
        } => {
            find_and_check_assignments(diags, ctx, value);
            if *is_mut {
                ctx.insert(&name.value);
            }
        }
        Expr::Block { body, .. } => {
            ctx.enter_scope();
            body.iter()
                .for_each(|e| find_and_check_assignments(diags, ctx, e));
            ctx.leave_scope();
        }
        Expr::Ret { value, .. } => {
            if let Some(e) = value {
                find_and_check_assignments(diags, ctx, e);
            }
        }
    }
}

fn check_decl(diags: &mut Vec<Diagnostic>, decl: &FnDecl) {
    let mut ctx = ScopedSet::with_scope(
        decl.params
            .iter()
            .filter(|p| p.is_mut)
            .map(|p| p.name.value.as_str())
            .collect(),
    );

    find_and_check_assignments(diags, &mut ctx, &decl.body)
}

impl Pass for AssignmentCorrectnessCheck {
    type Input = TranslationUnit;
    type Output = TranslationUnit;

    fn run(&mut self, (ast, typemap): Self::Input) -> Res<Self::Output> {
        let mut diags = vec![];

        ast.values().for_each(|decl| check_decl(&mut diags, decl));

        if diags.is_empty() {
            Res::Ok((ast, typemap))
        } else {
            Res::Fatal(diags)
        }
    }
}
