use std::iter::Peekable;
use std::process::exit;
use std::{ops::Index, str::CharIndices};

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

type It<'a> = Peekable<CharIndices<'a>>;

struct LexerCtx {
    line: usize,
    ret: Vec<Lexeme>,
}

impl LexerCtx {
    fn new() -> Self {
        LexerCtx {
            line: 0,
            ret: Vec::new(),
        }
    }
    fn new_line(&mut self) {
        self.line += 1;
    }

    fn add_lexeme(&mut self, lexeme: Lexeme) {
        self.ret.push(lexeme);
    }
}

fn get_location(tok: usize, ctx: &LexerCtx) -> Location {
    Location::new(ctx.line, tok)
}

fn num<'a>(mut it: It<'a>, ctx: &mut LexerCtx) -> It<'a> {
    let &(i, _) = it.peek().unwrap();

    let start_location = get_location(i, ctx);

    let mut end = 0;
    let number_string: String = it
        .by_ref()
        .take_while(|&(_, ch)| {
            end += 1;
            ch.is_numeric()
        })
        .map(|(_, ch)| ch)
        .collect();

    let end_location = get_location(i + end, ctx);

    let span = Span::new(start_location, end_location);

    let num = number_string.parse::<i64>();

    let num = match num {
        Ok(num) => num,
        Err(error) => {
            println!("{}", error);
            exit(1);
        }
    };

    let token = Token::Num(num);

    let lexeme = Lexeme::new(token, span);

    ctx.add_lexeme(lexeme);

    it
}

fn matcher<'a>(mut it: It<'a>, ctx: &mut LexerCtx) {
    while let Some((_, ch)) = it.next() {
        println!("{}", ch as u64);
        if ch == '\n' {
            println!("newline!");
            ctx.new_line();
            continue;
        }
        if ch.is_whitespace() {
            continue;
        }
        if ch.is_numeric() {
            it = num(it, ctx);
        }
    }
}

fn perform(s: String) -> LexerCtx {
    let iter = s.char_indices().peekable();
    let mut ctx = LexerCtx::new();

    matcher(iter, &mut ctx);

    ctx
}

pub fn lex(s: String) -> Vec<Lexeme> {
    perform(s).ret
}
