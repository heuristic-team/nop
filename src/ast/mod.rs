use std::collections::HashMap;
use std::fmt::Display;

use crate::lexer::{Span, WithSpan};
use crate::typesystem::types::*;

pub mod print;

pub type AST = HashMap<String, FnDecl>;

pub type FnParam = (WithSpan<String>, WithSpan<Type>);

#[derive(Debug)]
pub struct FnDecl {
    pub name: WithSpan<String>, // TODO: do we really need this field?
    pub tp: WithSpan<Type>,
    pub params: Vec<FnParam>,
    pub body: Block,
}

pub type Block = Vec<Stmt>;

#[derive(Debug, Clone)]
pub enum Stmt {
    Declare {
        name: WithSpan<String>,
        tp: WithSpan<Type>,
        value: Expr,
    },
    Expr(Expr),
}

pub type OpPrecedence = u8;

// #[derive(Debug, Clone, Copy)]
// pub enum UnaryOp {
//     Negate,
// }

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Plus,
    Minus,
    Mul,
}

impl BinaryOp {
    pub fn prec(&self) -> OpPrecedence {
        match self {
            Self::Plus => 4,
            Self::Minus => 4,
            Self::Mul => 5,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Num {
        tp: Type,
        value: WithSpan<u64>,
    },
    Ref {
        tp: Type,
        name: WithSpan<String>,
    },
    Call {
        tp: Type,
        callee: Box<Expr>,
        args: Vec<Expr>,
    },
    // Unary {
    //     op: UnaryOp,
    //     operand: Box<Expr>,
    // },
    Binary {
        tp: Type,
        op: WithSpan<BinaryOp>,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
}

#[derive(Debug, Clone)]
pub struct Loop {
    pub decl: Option<Vec<Stmt>>,
    pub cond: Option<Vec<Expr>>,
    pub on_iter: Option<Vec<Expr>>,
    pub span: Span,
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOp::Plus => write!(f, "+"),
            BinaryOp::Minus => write!(f, "-"),
            BinaryOp::Mul => write!(f, "*"),
        }
    }
}
