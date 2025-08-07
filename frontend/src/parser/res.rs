use crate::{Diagnostic, lexer::Span};

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

fn format_message<E: AsRef<str>>(expected: &[E], actual: &str) -> String {
    let mut res = String::new();

    res.push_str("expected ");
    let (last, init) = expected
        .split_last()
        .expect("`expected` should not be empty");

    if let Some((one_before_last, init)) = init.split_last() {
        init.iter().for_each(|e| {
            res.push_str(e.as_ref());
            res.push_str(", ")
        });

        res.push_str(one_before_last.as_ref());
    }

    if !init.is_empty() {
        res.push_str(" or ");
    }
    res.push_str(last.as_ref());
    res.push_str(", but got ");
    res.push_str(actual);

    res
}

impl Into<Diagnostic> for ParseError {
    fn into(self) -> Diagnostic {
        Diagnostic::new(format_message(&self.expected, &self.actual), self.span)
    }
}

pub type Res<T> = Result<T, ParseError>;
