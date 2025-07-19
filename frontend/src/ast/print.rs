use crate::ast::*;

pub trait TreePrintable {
    fn make_offset(depth: u8) {
        for _ in 0..depth {
            print!("  ");
        }
    }

    fn print(&self, depth: u8);
    fn print_top_level(&self) {
        self.print(0);
    }
}

impl TreePrintable for FnDecl {
    fn print(&self, depth: u8) {
        Self::make_offset(depth);

        print!("fn {}(", self.name.value);
        if let Some(((name, tp), init)) = self.params.split_last() {
            for (name, tp) in init {
                print!("{}: {}, ", name.value, tp.value);
            }
            print!("{}: {}", name.value, tp.value);
        }
        println!(") {} =", self.tp.value);

        for stmt in &self.body {
            stmt.print(depth + 1);
        }
    }
}

impl TreePrintable for Stmt {
    fn print(&self, depth: u8) {
        match self {
            Stmt::Declare {
                is_mut,
                name,
                tp,
                value,
            } => {
                Self::make_offset(depth);
                println!(
                    "let{} {}: {} =",
                    if *is_mut { " mut" } else { "" },
                    name.value,
                    tp.value
                );
                value.print(depth + 1);
            }
            Stmt::Expr(expr) => expr.print(depth),
        }
    }
}

impl TreePrintable for Expr {
    fn print(&self, depth: u8) {
        Self::make_offset(depth);
        match self {
            Expr::Num { tp, value } => println!("{} of type {}", value.value, tp),
            Expr::Ref { tp, name } => println!("{} of type {}", name.value, tp),
            Expr::Call { tp, callee, args } => {
                println!("call with result type {tp}");
                callee.print(depth + 1);
                Self::make_offset(depth + 1);
                println!("with args");
                for arg in args {
                    arg.print(depth + 2);
                }
            }
            Expr::Binary { tp, op, lhs, rhs } => {
                println!("{} with result type {tp}", op.value);
                lhs.print(depth + 1);
                rhs.print(depth + 1);
            }
        }
    }
}
