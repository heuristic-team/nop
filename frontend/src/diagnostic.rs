use crate::lexer::{Span, WithSpan};

pub type Note = WithSpan<String>;

#[derive(Debug)]
pub struct Diagnostic {
    pub msg: String,
    pub span: Span,
    pub notes: Vec<Note>,
}

impl Diagnostic {
    pub fn new(msg: String, span: Span) -> Self {
        Diagnostic {
            msg,
            span,
            notes: vec![],
        }
    }

    pub fn new_with_notes(msg: String, span: Span, notes: Vec<WithSpan<String>>) -> Self {
        Diagnostic { msg, span, notes }
    }
}

