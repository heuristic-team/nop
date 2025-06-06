#[derive(Debug)]
pub enum Token {
    Id(String),
    Num(i64),
    ScopeStart,
    ScopeEnd,
}

#[derive(Debug)]
struct Location {
    line: usize,
    symbol: usize,
}

impl Location {
    fn new(line: usize, symbol: usize) -> Self {
        Location { line, symbol }
    }
}

#[derive(Debug)]
struct Span {
    start: Location,
    end: Location,
}

impl Span {
    fn new(start: Location, end: Location) -> Self {
        Span { start, end }
    }
}

#[derive(Debug)]
pub struct Lexeme {
    token: Token,
    span: Span,
}

impl Lexeme {
    fn new(token: Token, span: Span) -> Self {
        Lexeme { token, span }
    }
}

pub fn lex(s: &str) -> Vec<Lexeme> {
    todo!()
}
