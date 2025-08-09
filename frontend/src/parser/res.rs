use crate::{Diagnostic, lexer::Span};

pub type Res<T> = Result<T, ParseError>;

/// Custom type for parse error.
///
/// Should not be created manually in *most* of the cases, `get` or `get_mut` should produce these errors. Exceptions are some tricky situations like trailing comma.
///
/// Has an implementation of `Into<Diagnostic>` to be passed to the driver and printed.
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

    /// Replace `expected` with given list.
    pub fn expect(mut self, expected: Vec<String>) -> Self {
        self.expected = expected;
        self
    }
}

/// Create a prettified message from a list of expected items and the actual item.
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
