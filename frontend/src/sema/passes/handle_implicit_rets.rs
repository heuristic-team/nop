use super::{Pass, Res};
use crate::ast::*;
use crate::typesystem::Type;

pub struct HandleImplicitRets {}

impl Pass for HandleImplicitRets {
    type Input = Vec<FnDecl>;
    type Output = Vec<FnDecl>;

    fn run(&mut self, mut decls: Self::Input) -> Res<Self::Output> {
        decls.iter_mut().for_each(process_decl);
        Res::Ok(decls)
    }
}

/// Replace implicit returns with explicit ones
fn process_decl(decl: &mut FnDecl) {
    let last_expr = find_implicit_ret_candidate(&mut decl.body);

    if matches!(last_expr, Expr::Ret { .. }) {
        return;
    }

    let span = last_expr.span();
    *last_expr = match &decl.tp.value {
        Type::Unit => Expr::Block {
            tp: Type::Unit,
            span: last_expr.span(),
            body: vec![
                last_expr.clone(), // TODO: remove this clone :( pt.1
                Expr::Ret { value: None, span },
            ],
        },
        _ => Expr::Ret {
            value: Some(Box::new(last_expr.clone())), // TODO: remove this clone :( pt.2
            span,
        },
    };
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
