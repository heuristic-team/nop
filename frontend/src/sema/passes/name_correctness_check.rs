use std::collections::HashMap;

use super::{Pass, Res};
use crate::Diagnostic;
use crate::TranslationUnit;
use crate::TypeAliasMap;
use crate::ast::*;
use crate::lexer::WithSpan;
use crate::support::ScopedSet;

pub struct NameCorrectnessCheck {}

type Names<'a> = ScopedSet<&'a str>;

/// Recursively go through expression tree and check that all references are valid,
/// adding a diagnostic otherwise
fn check_references_in_expr<'a>(diags: &mut Vec<Diagnostic>, ctx: &mut Names<'a>, expr: &'a Expr) {
    match expr {
        Expr::Ref { name, .. } if !ctx.contains(name.value.as_str()) => {
            diags.push(Diagnostic::new(
                format!("reference to undefined name {}", name.value),
                name.span,
            ));
        }
        Expr::Num { .. } | Expr::Ref { .. } | Expr::Bool { .. } => {}
        Expr::While { cond, body, .. } => {
            check_references_in_expr(diags, ctx, cond);
            check_references_in_expr(diags, ctx, body);
        }
        Expr::If {
            cond,
            on_true,
            on_false,
            ..
        } => {
            check_references_in_expr(diags, ctx, cond);
            check_references_in_expr(diags, ctx, on_true);
            if let Some(on_false) = on_false {
                check_references_in_expr(diags, ctx, on_false);
            }
        }
        Expr::Call { callee, args, .. } => {
            check_references_in_expr(diags, ctx, callee);
            for arg in args {
                check_references_in_expr(diags, ctx, arg);
            }
        }
        Expr::Binary { lhs, rhs, .. } => {
            check_references_in_expr(diags, ctx, lhs);
            check_references_in_expr(diags, ctx, rhs);
        }
        Expr::Declare { name, .. } => {
            ctx.insert(&name.value);
        }
        Expr::Block { body, .. } => {
            ctx.enter_scope();
            body.iter()
                .for_each(|e| check_references_in_expr(diags, ctx, e));
            ctx.leave_scope();
        }
        Expr::Ret { value, .. } => {
            if let Some(e) = value {
                check_references_in_expr(diags, ctx, e);
            }
        }
    }
}

/// Check declaration body for validity of references
fn check_references_in_decl<'a>(
    diags: &mut Vec<Diagnostic>,
    ctx: &mut Names<'a>,
    decl: &'a FnDecl,
) {
    ctx.enter_scope();
    for FnParam { name, .. } in decl.params.iter() {
        // TODO: check that parameters names are unique in the list
        ctx.insert(&name.value);
    }

    check_references_in_expr(diags, ctx, &decl.body);
    ctx.leave_scope();
}

impl Pass for NameCorrectnessCheck {
    type Input = (Vec<FnDecl>, TypeAliasMap);
    type Output = TranslationUnit;

    fn run(&mut self, (decls, typemap): Self::Input) -> Res<Self::Output> {
        let mut ast: AST = HashMap::new();
        let mut diags = Vec::new();

        for decl in decls.into_iter() {
            let prev = ast.get(&decl.name.value);

            if let Some(prev_decl) = prev {
                let note =
                    WithSpan::new("previously declared here".to_string(), prev_decl.name.span);
                let msg = format!("redeclaration of function {}", decl.name.value);
                diags.push(Diagnostic::new_with_notes(msg, decl.name.span, vec![note]));
            } else {
                ast.insert(decl.name.value.clone(), decl);
            }
        }

        let mut ctx = ScopedSet::new();
        ctx.add_scope(ast.keys().map(|s| s.as_str()).collect());
        for decl in ast.values() {
            check_references_in_decl(&mut diags, &mut ctx, decl);
        }

        if diags.is_empty() {
            Res::Ok((ast, typemap))
        } else {
            Res::Fatal(diags)
        }
    }
}
