use std::fmt::Display;

use crate::ir::{instr::BinaryType, ir_type::Typed};

use super::{
    basic_block::BasicBlock,
    function::Func,
    instr::{CmpType, Instr},
    operand::{Const, Label, Op, Var},
    program::Program,
};

///! File with implementations of [`Display`] trait for IR entities.

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for func in &self.fns {
            write!(f, "{}", func.borrow())?;
        }
        Ok(())
    }
}

impl Display for Func {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn {}: ", self.name)?;
        for param in &self.params {
            write!(f, "{} {} -> ", param.name, param.tp)?;
        }
        writeln!(f, "{}", self.tp)?;
        for block in &self.blocks {
            writeln!(f, "{}", block.borrow())?;
        }
        Ok(())
    }
}

impl Display for BasicBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}:", self.name)?;
        for instr in &self.instrs {
            writeln!(f, "  {}", instr)?;
        }
        Ok(())
    }
}

impl Display for CmpType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::GE => write!(f, ">="),
            Self::GT => write!(f, ">"),
            Self::LE => write!(f, "<="),
            Self::LT => write!(f, "<"),
            Self::EQ => write!(f, "=="),
            Self::NEQ => write!(f, "!="),
        }
    }
}

impl Display for BinaryType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryType::Add => write!(f, "add"),
            BinaryType::Mul => write!(f, "mul"),
            BinaryType::Sub => write!(f, "sub"),
            BinaryType::Div => write!(f, "div,"),
        }
    }
}

impl Display for Instr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ret(opt) => match opt {
                Some(op) => write!(f, "ret {} {}", op.get_type(), op),
                None => write!(f, "ret _"),
            },
            Self::Cmp { tp, dest, lhs, rhs } => {
                write!(
                    f,
                    "{} = {} cmp {} {} {}",
                    dest,
                    tp,
                    self.get_type(),
                    lhs,
                    rhs
                )
            }
            Self::Const { dest, imm } => {
                write!(f, "{} = {} const {}", dest, self.get_type(), imm)
            }
            Self::Jmp(label) => {
                write!(f, "jmp {}", label)
            }
            Self::Br {
                true_branch,
                false_branch,
                cond,
            } => {
                write!(
                    f,
                    "branch {}, true: {}, false: {}",
                    cond, true_branch, false_branch
                )
            }
            Self::Call { func, dest, args } => {
                write!(f, "{} = {} call {}", dest, self.get_type(), func)?;
                for arg in args {
                    write!(f, ", {}", arg)?;
                }
                Ok(())
            }
            Self::Binary { tp, dest, lhs, rhs } => {
                write!(f, "{} = {} {} {}, {}", dest, self.get_type(), tp, lhs, rhs)
            }
        }
    }
}

impl Display for Var {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(fmt, "{}", self.name)
    }
}

impl Display for Label {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Fn(func) => write!(fmt, "fn {}", func.name),
            Self::Block(block) => write!(fmt, "label {}", block.name),
        }
    }
}

impl Display for Const {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(int) => write!(fmt, "{}", int),
            Self::Bool(boolean) => write!(fmt, "{}", boolean),
        }
    }
}

impl Display for Op {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Variable(var) => write!(fmt, "{}", var),
            Self::Const(c) => write!(fmt, "{}", c),
        }
    }
}
