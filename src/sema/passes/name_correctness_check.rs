use std::collections::HashMap;
use std::collections::HashSet;

use super::Pass;
use super::Res;
use crate::ast::*;
use crate::lexer::WithSpan;
use crate::sema::Diagnostic;

pub struct NameCorrectnessCheck {}

type Ctx<'a> = HashSet<&'a str>;

fn check_expr<'a>(ctx: &Ctx<'a>, expr: &'a Expr) -> Vec<Diagnostic> {
    match expr {
        Expr::Num { .. } => vec![],
        Expr::Ref { name, .. } => {
            if !ctx.contains(name.value.as_str()) {
                vec![Diagnostic::new(
                    format!("reference to undefined name {}", name.value),
                    name.span,
                    vec![],
                )]
            } else {
                vec![]
            }
        }
        Expr::Call { callee, args, .. } => {
            let mut res = check_expr(ctx, callee);
            for arg in args {
                res.append(&mut check_expr(ctx, arg));
            }
            res
        }
        Expr::Binary { lhs, rhs, .. } => {
            let mut res = check_expr(ctx, lhs);
            res.append(&mut check_expr(ctx, rhs));
            res
        }
    }
}

fn check_decl<'a>(mut ctx: Ctx<'a>, decl: &'a FnDecl) -> Vec<Diagnostic> {
    let mut res = Vec::new();

    for FnParam {
        name: WithSpan { value: name, .. },
        ..
    } in decl.params.iter()
    {
        ctx.insert(&name);
    }

    for stmt in &decl.body {
        let mut new_diags = match stmt {
            Stmt::Declare { name, value, .. } => {
                let new_diags = check_expr(&ctx, value);
                ctx.insert(&name.value);
                new_diags
            }
            Stmt::Expr(expr) => check_expr(&ctx, expr),
        };

        res.append(&mut new_diags);
    }

    res
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
                diags.push(Diagnostic::new(msg, decl.name.span, vec![note]));
            } else {
                res.insert(decl.name.value.clone(), decl);
            }
        }

        let ctx = res.keys().map(|s| s.as_str()).collect::<Ctx>();
        for decl in res.values() {
            let mut new_diags = check_decl(ctx.clone(), decl);
            diags.append(&mut new_diags);
        }

        if diags.is_empty() {
            Res::Ok(res)
        } else {
            Res::Fatal(diags)
        }
    }
}
