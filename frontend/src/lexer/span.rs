/// Source file span. Basically a pair of offsets used for diagnostic printing.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Span { start, end }
    }
}

/// Value paired with source file span.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WithSpan<T> {
    pub value: T,
    pub span: Span,
}

impl<T> WithSpan<T> {
    pub fn new(value: T, span: Span) -> Self {
        Self { value, span }
    }

    /// Replace the value, leaving the span unchanged.
    pub fn replace<U>(self, value: U) -> WithSpan<U> {
        WithSpan {
            value,
            span: self.span,
        }
    }

    /// Apply a function to the value, leaving the span unchanged.
    pub fn map<U, F>(self, f: F) -> WithSpan<U>
    where
        F: FnOnce(T) -> U,
    {
        WithSpan {
            value: f(self.value),
            span: self.span,
        }
    }
}
