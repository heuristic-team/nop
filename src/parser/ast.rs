use crate::lexer::Span;
use crate::typesystem::types::*;

pub struct Fn {
    pub name: String,
    pub tp: Type,
    pub params: Vec<(String, Type)>,
    pub body: Block,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Block {
    pub body: Vec<Stmt>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Stmt {
    pub stmt: StmtContent,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub expr: ExprContent,
    pub tp: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum StmtContent {
    Declare(String, Expr),
    Assign(String, Expr),
    VoidExpr(Expr),
}

#[derive(Debug, Clone)]
pub struct Loop {
    pub decl: Option<Vec<Stmt>>,
    pub cond: Option<Vec<Expr>>,
    pub on_iter: Option<Vec<Expr>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ExprContent {
    Num(u64),
    Call(String, Vec<Expr>),
}
