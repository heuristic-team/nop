use crate::ast::Expr;

/// Utilitary function to visit the whole expression tree and run some context-free function on each node.
///
/// The function should be context-free because there is no options to, for example, enter and leave scopes when entering a block. This functionality needs a more sophisticated system to work.
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
