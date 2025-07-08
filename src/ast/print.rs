use crate::ast::*;

pub fn print_decl(decl: &FnDecl) {
    print!("fn {}(", decl.name);
    if let Some(((n, t), init)) = decl.params.split_last() {
        for (n, t) in init {
            print!("{n}: {t}, ");
        }
        print!("{n}: {t}")
    }
    println!(") {} =", decl.tp);

    for stmt in &decl.body {
        print_stmt(stmt, 1);
    }
}

fn make_offset(depth: u8) {
    for _ in 0..depth {
        print!("  ");
    }
}

fn print_stmt(stmt: &Stmt, depth: u8) {
    match stmt {
        Stmt::Declare { name, tp, value } => {
            make_offset(depth);
            println!("let {name}: {tp} =");
            print_expr(value, depth + 1);
        }
        Stmt::Expr(expr) => print_expr(expr, depth),
    }
}

fn print_expr(expr: &Expr, depth: u8) {
    make_offset(depth);
    match expr {
        Expr::Num { tp, value } => println!("{value} of type {tp}"),
        Expr::Ref { tp, name } => println!("{name} of type {tp}"),
        Expr::Call { callee, args } => {
            println!("call");
            print_expr(&callee, depth + 1);
            make_offset(depth + 1);
            println!("with args");
            for arg in args {
                print_expr(arg, depth + 2)
            }
            make_offset(depth);
        }
        Expr::Binary { op, lhs, rhs } => {
            println!("{op}");
            print_expr(lhs, depth + 1);
            print_expr(rhs, depth + 1);
        }
    }
}
