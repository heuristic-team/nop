use parser::It;

use crate::lexer::{Lexeme, Span};

mod ast;
mod parser;

pub fn parse(lexemes: Vec<Lexeme>) {
    let mut parser = It::new(lexemes);
    while let Some(lex) = parser.peekn(0) {
        println!("{:?}", lex);
        parser.next();
    }
}
