use crate::ast::*;

fn fmt_mut(is_mut: bool) -> &'static str {
    if is_mut { "mut " } else { "" }
}

pub fn print_decl(decl: &FnDecl) {
    fn print_param(param: &FnParam) {
        print!(
            "{}{}: {}",
            fmt_mut(param.is_mut),
            param.name.value,
            param.tp.value
        );
    }

    print!("fn {}(", decl.name.value);

    if let Some((last_param, init)) = decl.params.split_last() {
        init.iter().for_each(|param| {
            print_param(param);
            print!(", ");
        });
        print_param(last_param);
    }

    println!(") {} =", decl.tp.value);

    print_expr(&decl.body, 1);
}

fn make_offset(depth: u8) {
    for _ in 0..depth {
        print!("  ");
    }
}

fn print_expr(expr: &Expr, depth: u8) {
    make_offset(depth);
    match expr {
        Expr::Declare {
            is_mut,
            name,
            tp,
            value,
        } => {
            println!("Declare {}{} {} =", fmt_mut(*is_mut), name.value, tp.value);
            print_expr(value, depth + 1);
        }
        Expr::Num { tp, value } => println!("Num {} {}", value.value, tp),
        Expr::Ref { tp, name } => println!("Ref {} {}", name.value, tp),
        Expr::Call {
            tp, callee, args, ..
        } => {
            println!("Call {tp}");
            print_expr(&callee, depth + 1);
            make_offset(depth + 1);
            println!("with args");
            for arg in args {
                print_expr(arg, depth + 2)
            }
        }
        Expr::Binary { tp, op, lhs, rhs } => {
            println!("Binary {} {}", op.value, tp);
            print_expr(lhs, depth + 1);
            print_expr(rhs, depth + 1);
        }
        Expr::Block { body, tp } => {
            println!("Block {} {{", tp);
            body.iter().for_each(|e| print_expr(e, depth + 1));
            make_offset(depth);
            println!("}}");
        }
        Expr::Ret { value, .. } => {
            println!("Ret");
            if let Some(e) = value {
                print_expr(e, depth + 1);
            }
        }
    }
}
