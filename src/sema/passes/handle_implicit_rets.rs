use super::{Pass, Res};
use crate::ast::*;
use crate::typesystem::types::Type;

pub struct HandleImplicitRets {}

impl Pass for HandleImplicitRets {
    type Input = Vec<FnDecl>;
    type Output = Vec<FnDecl>;

    fn run(&mut self, mut decls: Self::Input) -> Res<Self::Output> {
        decls.iter_mut().for_each(process_decl);
        Res::Ok(decls)
    }
}

fn process_decl(decl: &mut FnDecl) {
    let last_expr = last_expr(&mut decl.body);

    if matches!(last_expr, Expr::Ret { .. }) {
        return;
    }

    let span = last_expr.span();
    *last_expr = match &decl.tp.value {
        Type::Unit => Expr::Block {
            tp: Type::Unit,

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

fn last_expr(e: &mut Expr) -> &mut Expr {
    match e {
        Expr::Declare { .. }
        | Expr::Ret { .. }
        | Expr::Num { .. }
        | Expr::Bool { .. }
        | Expr::Ref { .. }
        | Expr::Call { .. }
        | Expr::Binary { .. } => e,
        Expr::Block { body, .. } if body.is_empty() => e,
        Expr::Block { body, .. } => body.last_mut().map(last_expr).unwrap(),
    }
}
