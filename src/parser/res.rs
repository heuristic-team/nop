use crate::lexer::Span;

#[derive(Debug)]
pub struct ParseError {
    pub expected: Vec<String>,
    pub actual: String,
    pub span: Span,
}

impl ParseError {
    pub fn new(expected: Vec<String>, actual: String, span: Span) -> Self {
        Self {
            expected,
            actual,
            span,
        }
    }
}

pub type Res<T> = Result<T, ParseError>;
