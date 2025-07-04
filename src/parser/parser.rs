use crate::lexer::Lexeme;
use crate::lexer::Token;
use crate::parser::ast::ExprContent;
use crate::parser::parse;
use crate::typesystem::types::Type;

use super::ast::Expr;

pub struct It {
    lexems: Vec<Lexeme>,
    offset: usize,
}

pub struct ParseError {
    place: Lexeme,
    expected: Token,
    got: Token,
}

impl ParseError {
    pub fn new(place: Lexeme, expected: Token, got: Token) -> Self {
        ParseError {
            place,
            expected,
            got,
        }
    }
}

// We thought about it.
//enum Res<Type> {
//    Empty,
//    Ok(Type),
//    Error(ParseError),
//}

type Res<Type> = Option<Result<Type, ParseError>>;

fn expect(expected: Token, got: Token) -> Result<Token, ParseError> {}

impl It {
    pub fn new(lexems: Vec<Lexeme>) -> Self {
        It { lexems, offset: 0 }
    }

    fn check_len(&self, length: usize) -> bool {
        if length >= self.lexems.len() {
            false
        } else {
            true
        }
    }

    pub fn next(&mut self) -> Option<&Lexeme> {
        if !self.check_len(self.offset) {
            return None;
        }
        self.offset += 1;
        Some(&self.lexems[self.offset - 1])
    }

    pub fn peek(&mut self) -> Option<&Lexeme> {
        self.peekn(0)
    }

    pub fn peekn(&mut self, n: usize) -> Option<&Lexeme> {
        if !self.check_len(self.offset + n + 1) {
            return None;
        }
        Some(&self.lexems[self.offset + n])
    }
}

fn expr(it: &mut It) {
    //
}

fn expected(expected: Token, got: Lexeme) -> Result<Lexeme, ParseError> {
    if expected == got {
        Ok(got)
    } else {
        ParseError::new(got.span, expected, got)
    }
}

fn parse_call(it: &mut It) -> Res<Expr> {
    let id = it.peek();
    if let None = id {
        return None;
    }

    let left_bracket = it.peekn(1);
    if let None = left_bracket {
        return None;
    }

    let call_name = it.next().unwrap();
    let _left_bracket = it.next();

    while let Some(token) = it.next()
        && token != Token::RParen
    {
        let lexeme = expected(Token::Id, token)?; // IDK TODO CHECK!
        //let arg = Expr::new(ExprContent::Ref(lexeme))
        it.next()
    }
    todo!()
}

fn parse_num(it: &mut It) -> Res<Expr> {
    if let Some(peek) = it.peek() {
        let num = match peek.token {
            Token::Num(num) => ExprContent::Num(num),
            _ => return None,
        };
        let span = peek.span;
        it.next();
        Some(Ok(Expr::new(num, Type::I64, span)))
    } else {
        None
    }
}
