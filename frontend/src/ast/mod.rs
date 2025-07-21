use std::collections::HashMap;
use std::fmt::Display;

use crate::lexer::{Span, WithSpan};
use crate::typesystem::*;

pub mod print;

pub type AST = HashMap<String, FnDecl>;

#[derive(Debug)]
pub struct FnParam {
    pub is_mut: bool,
    pub name: WithSpan<String>,
    pub tp: WithSpan<Type>,
}

#[derive(Debug)]
pub struct FnDecl {
    pub name: WithSpan<String>,
    pub tp: WithSpan<Type>,
    pub params: Vec<FnParam>,
    pub body: Expr,
}

impl FnDecl {
    pub fn formal_type(&self) -> Type {
        let params = self.params.iter().map(|p| &p.tp.value).cloned().collect();
        let rettype = Box::new(self.tp.value.clone());
        Type::Function { params, rettype }
    }
}

pub type OpPrecedence = u8;

// #[derive(Debug, Clone, Copy)]
// pub enum UnaryOp {
//     Negate,
// }

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Assign,
    Plus,
    Minus,
    Mul,
}

impl BinaryOp {
    pub fn prec(&self) -> OpPrecedence {
        match self {
            Self::Assign => 1,
            Self::Plus => 4,
            Self::Minus => 4,
            Self::Mul => 5,
        }
    }

    pub fn is_cmp(&self) -> bool {
        match self {
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Declare {
        is_mut: bool,
        name: WithSpan<String>,
        tp: WithSpan<Type>,
        value: Box<Expr>,
    },
    Ret {
        value: Option<Box<Expr>>,
        span: Span,
    },
    Block {
        tp: Type,
        body: Vec<Expr>,
        span: Span,
    },
    While {
        // tp: Type, // TODO, see issue #18
        cond: Box<Expr>,
        body: Box<Expr>,
        span: Span,
    },
    If {
        tp: Type,
        cond: Box<Expr>,
        on_true: Box<Expr>,
        on_false: Option<Box<Expr>>,
        kw_span: Span,
        in_stmt_pos: bool,
    },
    Num {
        tp: Type,
        value: WithSpan<u64>,
    },
    Bool {
        value: bool,
        span: Span,
    },
    Ref {
        tp: Type,
        name: WithSpan<String>,
    },
    Call {
        tp: Type,
        callee: Box<Expr>,
        args: Vec<Expr>,
        span: Span,
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

impl Expr {
    pub fn tp(&self) -> &Type {
        match self {
            Expr::Num { tp, .. }
            | Expr::Ref { tp, .. }
            | Expr::Call { tp, .. }
            | Expr::Binary { tp, .. }
            | Expr::Block { tp, .. }
            | Expr::If { tp, .. } => tp,
            Expr::Bool { .. } => &Type::Bool,
            Expr::Declare { .. } => &Type::Unit,
            Expr::Ret { .. } => &Type::Bottom,
            Expr::While { .. } => &Type::Unit, // to change, see issue #18
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Expr::Block { span, .. } => *span,
            Expr::Bool { span, .. } => *span,
            Expr::Num { value, .. } => value.span,
            Expr::Ref { name, .. } => name.span,
            Expr::Call { span, .. } => *span,
            Expr::While { span, .. } => *span,
            Expr::If {
                kw_span,
                on_true,
                on_false: None,
                ..
            } => Span::new(kw_span.start, on_true.span().end),
            Expr::If {
                kw_span,
                on_false: Some(on_false),
                ..
            } => Span::new(kw_span.start, on_false.span().end),
            Expr::Binary { lhs, rhs, .. } => Span::new(lhs.span().start, rhs.span().end),
            Expr::Declare { name, value, .. } => Span::new(name.span.start, value.span().end),
            Expr::Ret { span, .. } => *span,
        }
    }
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOp::Assign => write!(f, "="),
            BinaryOp::Plus => write!(f, "+"),
            BinaryOp::Minus => write!(f, "-"),
            BinaryOp::Mul => write!(f, "*"),
        }
    }
}
