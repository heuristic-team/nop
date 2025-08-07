use std::slice::IterMut;

use super::Token;
use super::{Span, WithSpan};

pub type Lexeme = WithSpan<Token>;
pub type LexemesState = usize;

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

impl<'a> IntoIterator for &'a mut Lexemes {
    type Item = &'a mut Lexeme;
    type IntoIter = IterMut<'a, Lexeme>;

    fn into_iter(self) -> Self::IntoIter {
        self.lexemes.as_mut_slice().into_iter()
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
            .cloned()
            .unwrap_or(self.eof())
    }

    pub fn peek_n<const N: usize>(&self) -> [Lexeme; N] {
        core::array::from_fn(|i| self.peek_nth(i))
    }

    pub fn next(&mut self) -> Lexeme {
        let res = self.peek();
        self.skip_n(1);
        res
    }

    pub fn skip_n(&mut self, n: usize) {
        self.offset = (self.offset + n).clamp(0, self.lexemes.len());
    }

    pub fn get_state(&self) -> LexemesState {
        self.offset
    }

    pub fn set_state(&mut self, state: LexemesState) {
        self.offset = state;
    }
}
