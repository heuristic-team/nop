use crate::ast::*;

impl FnDecl {
    fn print_param(param: &FnParam) {
        print!(
            "{}{}: {}",
            fmt_mut(param.is_mut),
            param.name.value,
            param.tp.value
        );
    }

    pub fn print(&self) {
        print!("fn {}(", self.name.value);

        if let Some((last_param, init)) = self.params.split_last() {
            init.iter().for_each(|param| {
                Self::print_param(param);
                print!(", ");
            });
            Self::print_param(last_param);
        }

        println!(") {} =", self.return_type.value);

        self.body.print(1);
    }
}

impl Expr {
    pub fn print(&self, depth: u8) {
        make_offset(depth);
        match self {
            Expr::Declare {
                is_mut,
                name,
                tp,
                value,
            } => {
                println!("Declare {}{} {} =", fmt_mut(*is_mut), name.value, tp.value);
                value.print(depth + 1);
            }
            Expr::While { cond, body, .. } => {
                println!("While");
                cond.print(depth + 1);
                make_offset(depth);
                println!("do");
                body.print(depth + 1);
            }
            Expr::If {
                tp,
                cond,
                on_true,
                on_false,
                ..
            } => {
                println!("If {}", tp);
                cond.print(depth + 1);
                make_offset(depth);
                println!("then");
                on_true.print(depth + 1);

                if let Some(on_false) = on_false {
                    make_offset(depth);
                    println!("else");
                    on_false.print(depth + 1);
                }
            }
            Expr::Num { tp, value } => println!("Num {} {}", value.value, tp),
            Expr::Ref { tp, name } => println!("Ref {} {}", name.value, tp),
            Expr::MemberRef { tp, target, member } => {
                println!("MemberRef {} {}", member.value, tp);
                target.print(depth + 1);
            }
            Expr::Call {
                tp, callee, args, ..
            } => {
                println!("Call {tp}");
                callee.print(depth + 1);
                make_offset(depth);
                println!("with args");
                for arg in args {
                    arg.print(depth + 1)
                }
            }
            Expr::Binary { tp, op, lhs, rhs } => {
                println!("Binary {} {}", op.value, tp);
                lhs.print(depth + 1);
                rhs.print(depth + 1);
            }
            Expr::Block { body, tp, .. } => {
                println!("Block {} {{", tp);
                body.iter().for_each(|e| e.print(depth + 1));
                make_offset(depth);
                println!("}}");
            }
            Expr::Ret { value, .. } => {
                println!("Ret");
                if let Some(e) = value {
                    e.print(depth + 1);
                }
            }
            Expr::Bool { value, .. } => println!("Bool {value}"),
        }
    }
}

fn make_offset(depth: u8) {
    for _ in 0..depth {
        print!("  ");
    }
}

fn fmt_mut(is_mut: bool) -> &'static str {
    if is_mut { "mut " } else { "" }
}
