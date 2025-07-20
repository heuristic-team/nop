use std::borrow::Cow;

use std::collections::HashMap;
use std::collections::HashSet;

use super::{Pass, Res};
use crate::Diagnostic;
use crate::ast::*;
use crate::lexer::WithSpan;

pub struct NameCorrectnessCheck {}

type Names<'a> = HashSet<&'a str>;

fn check_expr<'a>(diags: &mut Vec<Diagnostic>, ctx: &mut Cow<Names<'a>>, expr: &'a Expr) {
    match expr {
        Expr::Ref { name, .. } if !ctx.contains(name.value.as_str()) => {
            diags.push(Diagnostic::new(
                format!("reference to undefined name {}", name.value),
                name.span,
            ));
        }
        Expr::Num { .. } | Expr::Ref { .. } | Expr::Bool { .. } => {}
        Expr::Call { callee, args, .. } => {
            check_expr(diags, ctx, callee);
            for arg in args {
                check_expr(diags, ctx, arg);
            }
        }
        Expr::Binary { lhs, rhs, .. } => {
            check_expr(diags, ctx, lhs);
            check_expr(diags, ctx, rhs);
        }
        Expr::Declare { name, .. } => {
            ctx.to_mut().insert(&name.value);
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

fn check_decl<'a>(diags: &mut Vec<Diagnostic>, ctx: &mut Cow<Names<'a>>, decl: &'a FnDecl) {
    let mut ctx = ctx.clone();

    for FnParam {
        name: WithSpan { value: name, .. },
        ..
    } in decl.params.iter()
    {
        ctx.to_mut().insert(&name);
    }

    check_expr(diags, &mut ctx, &decl.body);
}

impl Pass for NameCorrectnessCheck {
    type Input = Vec<FnDecl>;
    type Output = AST;

    fn run(&mut self, decls: Self::Input) -> Res<Self::Output> {
        let mut res: Self::Output = HashMap::new();
        let mut diags = Vec::new();

        for decl in decls.into_iter() {
            let prev = res.get(&decl.name.value);

            if let Some(prev_decl) = prev {
                let note =
                    WithSpan::new("previously declared here".to_string(), prev_decl.name.span);
                let msg = format!("redeclaration of function {}", decl.name.value);
                diags.push(Diagnostic::new_with_notes(msg, decl.name.span, vec![note]));
            } else {
                res.insert(decl.name.value.clone(), decl);
            }
        }

        let ctx = res.keys().map(|s| s.as_str()).collect::<Names>();
        for decl in res.values() {
            check_decl(&mut diags, &mut Cow::Borrowed(&ctx), decl);
        }

        if diags.is_empty() {
            Res::Ok(res)
        } else {
            Res::Fatal(diags)
        }
    }
}
