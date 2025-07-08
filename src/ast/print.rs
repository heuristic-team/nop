use crate::ast::*;

pub fn print_decl(decl: &FnDecl) {
    print!("fn {}(", decl.name.value);
    if let Some(((name, tp), init)) = decl.params.split_last() {
        for (name, tp) in init {
            print!("{}: {}, ", name.value, tp.value);
        }
        print!("{}: {}", name.value, tp.value);
    }
    println!(") {} =", decl.tp.value);

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
            println!("let {}: {} =", name.value, tp.value);
            print_expr(value, depth + 1);
        }
        Stmt::Expr(expr) => print_expr(expr, depth),
    }
}

fn print_expr(expr: &Expr, depth: u8) {
    make_offset(depth);
    match expr {
        Expr::Num { tp, value } => println!("{} of type {}", value.value, tp),
        Expr::Ref { tp, name } => println!("{} of type {}", name.value, tp),
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
            println!("{}", op.value);
            print_expr(lhs, depth + 1);
            print_expr(rhs, depth + 1);
        }
    }
}
