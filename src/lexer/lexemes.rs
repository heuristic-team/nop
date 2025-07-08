use super::Token;
use super::{Span, WithSpan};

pub type Lexeme = WithSpan<Token>;

pub trait Lexemes: IntoIterator<Item = Lexeme> {
    fn is_eof(&self) -> bool;
    fn next(&mut self) -> Lexeme;
    fn peek(&self) -> Lexeme;
    fn nth(&mut self, n: usize) -> Lexeme;
    fn peek_nth(&self, n: usize) -> Lexeme;
}

pub struct DefaultLexemes {
    lexemes: Vec<Lexeme>,
    offset: usize,
    eof_span: Span,
}

impl DefaultLexemes {
    pub fn new(lexemes: Vec<Lexeme>, eof_span: Span) -> Self {
        Self {
            lexemes,
            offset: 0,
            eof_span,
        }
    }

    fn eof(&self) -> Lexeme {
        WithSpan::new(Token::EOF, self.eof_span.clone())
    }
}

impl<'a> IntoIterator for &'a DefaultLexemes {
    type Item = &'a Lexeme;
    type IntoIter = std::slice::Iter<'a, Lexeme>;

    fn into_iter(self) -> Self::IntoIter {
        self.lexemes.as_slice().into_iter()
    }
}

impl IntoIterator for DefaultLexemes {
    type Item = Lexeme;
    type IntoIter = std::vec::IntoIter<Lexeme>;

    fn into_iter(self) -> Self::IntoIter {
        self.lexemes.into_iter()
    }
}

impl Lexemes for DefaultLexemes {
    fn is_eof(&self) -> bool {
        self.offset >= self.lexemes.len()
    }

    fn peek(&self) -> Lexeme {
        self.peek_nth(0)
    }

    fn peek_nth(&self, n: usize) -> Lexeme {
        self.lexemes
            .get(self.offset + n)
            .map(|l| l.clone())
            .unwrap_or(self.eof())
    }

    fn next(&mut self) -> Lexeme {
        self.nth(0)
    }

    fn nth(&mut self, n: usize) -> Lexeme {
        let res = self.peek_nth(n);
        if let Token::EOF = res.value {
            self.offset = self.lexemes.len();
        } else {
            self.offset += n + 1;
        }
        res
    }
}
