use std::collections::HashMap;
use std::fmt::Display;
use std::rc::Rc;

use crate::lexer::{Span, WithSpan};
use crate::typesystem::*;

pub mod print;

pub type AST = HashMap<String, FnDecl>;

/// Function parameter description.
///
/// For example, function `fn foo(mut a: int, b: bool)` has two parameters: a mutable `a` of type `int` and immutable `b` of type `bool`.
#[derive(Debug)]
pub struct FnParam {
    pub is_mut: bool,
    pub name: WithSpan<String>,
    pub tp: WithSpan<Rc<Type>>,
}

/// Function declaration desription. A literal translation of the source code.
#[derive(Debug)]
pub struct FnDecl {
    pub name: WithSpan<String>,
    pub return_type: WithSpan<Rc<Type>>,
    pub params: Vec<FnParam>,
    pub body: Expr,
}

impl FnDecl {
    /// Return the type of function for typecheck.
    ///
    /// `FnDecl` itself contains the information gathered from the source code, such as parameters types and return type, but typecheck needs type like `fn(int, int) int` to process the call.
    pub fn full_type(&self) -> Type {
        let params = self.params.iter().map(|p| &p.tp.value).cloned().collect();
        let rettype = self.return_type.value.clone();
        Type::Function { params, rettype }
    }
}

pub type Precedence = u8;

pub enum Associativity {
    Left,
    Right,
}

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
    Eq,
    NotEq,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    And,
    Or,
}

impl BinaryOp {
    /// Binary operator precedence for parsing.
    pub fn prec(&self) -> Precedence {
        match self {
            Self::Assign => 1,
            Self::Or => 2,
            Self::And => 3,
            Self::Eq | Self::NotEq => 4,
            Self::Less | Self::LessEq | Self::Greater | Self::GreaterEq => 5,
            Self::Plus | Self::Minus => 6,
            Self::Mul => 7,
        }
    }

    /// Binary operator associativity for parsing.
    pub fn assoc(&self) -> Associativity {
        match self {
            Self::Assign => Associativity::Right,
            Self::Plus
            | Self::Minus
            | Self::Mul
            | Self::Eq
            | Self::NotEq
            | Self::Less
            | Self::LessEq
            | Self::Greater
            | Self::GreaterEq
            | Self::And
            | Self::Or => Associativity::Left,
        }
    }

    /// Check if the operator is some form of comparison, meaning it will return `bool`, instead of `T`.
    ///
    /// This may change when operators are handled as proper method calls.
    pub fn is_cmp(&self) -> bool {
        match self {
            Self::Eq
            | Self::NotEq
            | Self::Less
            | Self::LessEq
            | Self::Greater
            | Self::GreaterEq => true,
            _ => false,
        }
    }

    /// Check if the operator is logical, i.e. only applicable to bool arguments
    pub fn is_logical(&self) -> bool {
        match self {
            Self::And | Self::Or => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Declare {
        is_mut: bool,
        name: WithSpan<String>,
        tp: WithSpan<Rc<Type>>,
        value: Box<Expr>,
    },
    Ret {
        value: Option<Box<Expr>>,
        span: Span,
    },
    Block {
        tp: Rc<Type>,
        body: Vec<Expr>,
        span: Span,
    },
    While {
        // tp: Rc<Type>, // TODO, see issue #18
        cond: Box<Expr>,
        body: Box<Expr>,
        span: Span,
    },
    If {
        tp: Rc<Type>,
        cond: Box<Expr>,
        on_true: Box<Expr>,
        on_false: Option<Box<Expr>>,
        kw_span: Span,
        in_stmt_pos: bool,
    },
    Num {
        tp: Rc<Type>,
        value: WithSpan<u64>,
    },
    Bool {
        value: bool,
        span: Span,
    },
    Ref {
        tp: Rc<Type>,
        name: WithSpan<String>,
    },
    MemberRef {
        tp: Rc<Type>,
        target: Box<Expr>,
        member: WithSpan<String>,
    },
    Call {
        tp: Rc<Type>,
        callee: Box<Expr>,
        args: Vec<Expr>,
        span: Span,
    },
    // Unary {
    //     op: UnaryOp,
    //     operand: Box<Expr>,
    // },
    Binary {
        tp: Rc<Type>,
        op: WithSpan<BinaryOp>,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
}

impl Expr {
    /// Get an immutable reference to the type of the expression.
    ///
    /// See `tp_rc` if you not only need to inspect the type, but also put it somewhere in `Rc`.
    pub fn tp(&self) -> &Type {
        match self {
            Expr::Num { tp, .. }
            | Expr::Ref { tp, .. }
            | Expr::Call { tp, .. }
            | Expr::Binary { tp, .. }
            | Expr::Block { tp, .. }
            | Expr::If { tp, .. }
            | Expr::MemberRef { tp, .. } => tp,

            Expr::Bool { .. } => &Type::Bool,
            Expr::Declare { .. } => &Type::Unit,
            Expr::Ret { .. } => &Type::Bottom,
            Expr::While { .. } => &Type::Unit, // to change, see issue #18
        }
    }
    /// Get an `Rc` with the type of the expression.
    ///
    /// See `tp` if you only need to inspect the type.
    pub fn tp_rc(&self) -> Rc<Type> {
        match self {
            Expr::Num { tp, .. }
            | Expr::Ref { tp, .. }
            | Expr::Call { tp, .. }
            | Expr::Binary { tp, .. }
            | Expr::Block { tp, .. }
            | Expr::If { tp, .. }
            | Expr::MemberRef { tp, .. } => tp.clone(),

            Expr::Bool { .. } => Rc::new(Type::Bool),
            Expr::Declare { .. } => Rc::new(Type::Unit),
            Expr::Ret { .. } => Rc::new(Type::Bottom),
            Expr::While { .. } => Rc::new(Type::Unit), // to change, see issue #18
        }
    }

    /// Obtain the source file span corresponding to given expression. Used for diagnostics printing.
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
            Expr::MemberRef { member: member_name, .. } => member_name.span,
        }
    }
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Assign => write!(f, "="),
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Eq => write!(f, "=="),
            Self::NotEq => write!(f, "!="),
            Self::Less => write!(f, "<"),
            Self::LessEq => write!(f, "<="),
            Self::Greater => write!(f, ">"),
            Self::GreaterEq => write!(f, ">="),
            Self::And => write!(f, "&&"),
            Self::Or => write!(f, "||"),
        }
    }
}
