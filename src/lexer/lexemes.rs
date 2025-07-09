use super::Token;
use super::{Span, WithSpan};

pub type Lexeme = WithSpan<Token>;

#[derive(Debug)]
pub struct Lexemes {
    lexemes: Vec<Lexeme>,
    offset: usize,
    eof_span: Span,
}

impl<'a> IntoIterator for &'a Lexemes {
    type Item = &'a Lexeme;
    type IntoIter = std::slice::Iter<'a, Lexeme>;

    fn into_iter(self) -> Self::IntoIter {
        self.lexemes.as_slice().into_iter()
    }
}

impl IntoIterator for Lexemes {
    type Item = Lexeme;
    type IntoIter = std::vec::IntoIter<Lexeme>;

    fn into_iter(self) -> Self::IntoIter {
        self.lexemes.into_iter()
    }
}

impl Lexemes {
    pub fn new(lexemes: Vec<Lexeme>, eof_span: Span) -> Self {
        Self {
            lexemes,
            offset: 0,
            eof_span,
        }
    }

    pub fn is_eof(&self) -> bool {
        self.offset >= self.lexemes.len()
    }

    pub fn peek(&self) -> Lexeme {
        self.peek_nth(0)
    }

    fn eof(&self) -> Lexeme {
        WithSpan::new(Token::EOF, self.eof_span)
    }

    pub fn peek_nth(&self, n: usize) -> Lexeme {
        self.lexemes
            .get(self.offset + n)
            .map(|l| l.clone())
            .unwrap_or(self.eof())
    }

    fn advance(&mut self, n: usize) {
        self.offset = (self.offset + n).clamp(0, self.lexemes.len());
    }

    pub fn next(&mut self) -> Lexeme {
        let res = self.peek();
        self.advance(1);
        res
    }

    pub fn skip_n(&mut self, n: usize) {
        self.advance(n);
    }
}
