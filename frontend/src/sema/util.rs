use crate::ast::Expr;

/// Utilitary function to visit the whole expression tree and run some function on each node.
pub fn for_each_expr(f: &mut impl FnMut(&Expr), root: &Expr) {
    f(root);

    match root {
        Expr::Declare { value, .. } => for_each_expr(f, value),
        Expr::Ret { value, .. } => {
            if let Some(value) = value {
                for_each_expr(f, value);
            }
        }
        Expr::While { cond, body, .. } => {
            for_each_expr(f, cond);
            for_each_expr(f, body);
        }
        Expr::If {
            cond,
            on_true,
            on_false,
            ..
        } => {
            for_each_expr(f, cond);
            for_each_expr(f, on_true);
            if let Some(on_false) = on_false {
                for_each_expr(f, on_false);
            }
        }
        Expr::Block { body, .. } => body.iter().for_each(|e| for_each_expr(f, e)),
        Expr::Num { .. } | Expr::Ref { .. } | Expr::Bool { .. } => {}
        Expr::MemberRef { target, .. } => for_each_expr(f, target),
        Expr::Call { callee, args, .. } => {
            for_each_expr(f, callee);
            args.iter().for_each(|e| for_each_expr(f, e));
        }
        Expr::Binary { lhs, rhs, .. } => {
            for_each_expr(f, lhs);
            for_each_expr(f, rhs);
        }
    }
}

/// Utilitary function to visit the whole expression tree and run some function on each node.
pub fn for_each_expr_mut(f: &mut impl FnMut(&mut Expr), root: &mut Expr) {
    f(root);

    match root {
        Expr::Declare { value, .. } => for_each_expr_mut(f, value),
        Expr::Ret { value, .. } => {
            if let Some(value) = value {
                for_each_expr_mut(f, value);
            }
        }
        Expr::While { cond, body, .. } => {
            for_each_expr_mut(f, cond);
            for_each_expr_mut(f, body);
        }
        Expr::If {
            cond,
            on_true,
            on_false,
            ..
        } => {
            for_each_expr_mut(f, cond);
            for_each_expr_mut(f, on_true);
            if let Some(on_false) = on_false {
                for_each_expr_mut(f, on_false);
            }
        }
        Expr::Block { body, .. } => body.iter_mut().for_each(|e| for_each_expr_mut(f, e)),
        Expr::Num { .. } | Expr::Ref { .. } | Expr::Bool { .. } => {}
        Expr::MemberRef { target, .. } => for_each_expr_mut(f, target),
        Expr::Call { callee, args, .. } => {
            for_each_expr_mut(f, callee);
            args.iter_mut().for_each(|e| for_each_expr_mut(f, e));
        }
        Expr::Binary { lhs, rhs, .. } => {
            for_each_expr_mut(f, lhs);
            for_each_expr_mut(f, rhs);
        }
    }
}
