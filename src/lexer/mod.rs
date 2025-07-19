use std::iter::Peekable;
use std::str::CharIndices;

mod span;
pub use span::*;

mod token;
pub use token::Token;

mod lexemes;
pub use lexemes::{Lexeme, Lexemes};

const KEYWORDS: &[(Token, &str)] = &[
    (Token::Fn, "fn"),
    (Token::Type, "type"),
    (Token::Mut, "mut"),
    (Token::Ret, "ret"),
    (Token::True, "true"),
    (Token::False, "false"),
];

const SYMBOLS: &[(char, Token)] = &[
    ('(', Token::LParen),
    (')', Token::RParen),
    ('{', Token::LBrace),
    ('}', Token::RBrace),
    ('\"', Token::Quote),
    ('.', Token::Dot),
    (',', Token::Comma),
    ('+', Token::Plus),
    ('*', Token::Mul),
];

const MULTIPLE_CHAR_SYMBOLS: &[(char, Token, &'static str, Token)] = &[
    ('=', Token::Assign, "==", Token::Eq),
    (':', Token::Colon, ":=", Token::Define),
    ('-', Token::Minus, "->", Token::Arrow),
];

// #[derive(Debug, Clone)]
// pub struct Lexeme {
//     pub token: Token,
//     pub span: Span,
// }

// impl Lexeme {
//     fn new(token: Token, span: Span) -> Self {
//         Lexeme { token, span }
//     }
// }

type It<'a> = Peekable<CharIndices<'a>>;

struct LexerCtx {
    line: usize,
    file_size: usize,
    ret: Vec<Lexeme>,
}

impl LexerCtx {
    fn create_location(&self, tok: usize) -> Location {
        Location::new(self.line, tok)
    }

    fn current_location<'a>(&self, it: &mut It<'a>) -> Location {
        self.create_location(peek_location(it, self))
    }

    fn eof_span(&self) -> Span {
        let loc = Location::new(self.line, self.file_size);
        Span::new(loc, loc)
    }

    fn new(file_size: usize) -> Self {
        LexerCtx {
            line: 0,
            file_size,
            ret: Vec::new(),
        }
    }
    fn new_line<'a>(&mut self, it: &mut It<'a>) {
        let pos = self.current_location(it);
        let token = Token::EOL;
        let span = Span::new(pos, pos);
        let lexeme = Lexeme::new(token, span);
        self.add_lexeme(lexeme);
        self.line += 1;
    }

    fn add_lexeme(&mut self, lexeme: Lexeme) {
        self.ret.push(lexeme);
    }
}

fn peek_location<'a>(it: &mut It<'a>, ctx: &LexerCtx) -> usize {
    let loc = it.peek();
    if let Some(&(n, _)) = loc {
        n
    } else {
        ctx.file_size
    }
}

fn eat_while<'a>(f: fn(char) -> bool, it: &mut It<'a>) -> String {
    let mut ret = String::new();

    while let Some(&(_, ch)) = it.peek() {
        if !f(ch) {
            break;
        }
        ret.push(ch);
        it.next();
    }

    ret
}

fn lex_number<'a>(it: &mut It<'a>, ctx: &mut LexerCtx) {
    let start = ctx.current_location(it);

    let num = eat_while(char::is_numeric, it);

    let end = ctx.current_location(it);

    let span = Span::new(start, end);

    let token = Token::Num(num.parse().unwrap());

    let lexeme = Lexeme::new(token, span);

    ctx.add_lexeme(lexeme);
}

fn multiple_symbol<'a>(it: &mut It<'a>, expected: &str, expected_token: Token) -> Option<Token> {
    let mut cloned = it.clone();

    let len = expected.len();

    for i in expected.chars() {
        let peeked = cloned.peek();
        if peeked.is_none() {
            return None;
        }
        let &(_, ch) = peeked.unwrap();
        if ch != i {
            return None;
        }
        cloned.next();
    }

    for _ in 0..len {
        it.next();
    }

    Some(expected_token)
}

fn valid_id_start(ch: char) -> bool {
    ch.is_alphabetic() || ch == '_'
}

fn valid_id(ch: char) -> bool {
    ch.is_alphanumeric() || ch == '_'
}

fn single_symbol<'a>(it: &mut It<'a>, ctx: &LexerCtx) -> Option<Lexeme> {
    let &(_, symbol) = it.peek().unwrap();

    for (ch, token) in SYMBOLS {
        if *ch == symbol {
            let start = ctx.current_location(it);
            it.next();
            let end = ctx.current_location(it);

            let span = Span::new(start, end);

            let lexeme = Lexeme::new(token.clone(), span);

            return Some(lexeme);
        }
    }
    None
}

// redo it with returning and options instead of mutating lexer context.
fn lex_symbols<'a>(it: &mut It<'a>, ctx: &mut LexerCtx) -> Option<Lexeme> {
    let try_single = single_symbol(it, ctx);

    if try_single.is_some() {
        return try_single;
    }

    let start = ctx.current_location(it);

    let &(_, symbol) = it.peek().unwrap();

    if let Some((fallback_token, long_token_literal, long_token)) = MULTIPLE_CHAR_SYMBOLS
        .iter()
        .find(|&(c, _, _, _)| *c == symbol)
        .map(|(_, t1, lit, t2)| (t1, lit, t2))
    {
        let token = multiple_symbol(it, long_token_literal, long_token.clone());
        if token.is_none() {
            it.next();
        }

        let token = token.unwrap_or(fallback_token.clone());
        let end = ctx.current_location(it);
        let span = Span::new(start, end);
        let lexeme = Lexeme::new(token, span);

        return Some(lexeme);
    }

    None
}

fn lex_id<'a>(it: &mut It<'a>, ctx: &LexerCtx) -> Lexeme {
    let start = ctx.current_location(it);

    let id = eat_while(valid_id, it);

    let end = ctx.current_location(it);

    let span = Span::new(start, end);

    for (token, str) in KEYWORDS {
        if *str == id {
            return Lexeme::new(token.clone(), span);
        }
    }

    Lexeme::new(Token::Id(id), span)
}

fn matcher<'a>(it: &mut It<'a>, ctx: &mut LexerCtx) {
    while let Some(&(_, ch)) = it.peek() {
        if let Some(lexeme) = lex_symbols(it, ctx) {
            ctx.add_lexeme(lexeme);
        } else if valid_id_start(ch) {
            let id = lex_id(it, ctx);
            ctx.add_lexeme(id);
        } else if ch.is_numeric() {
            lex_number(it, ctx);
        } else if ch == '\n' {
            it.next();
            ctx.new_line(it);
        } else if ch.is_whitespace() {
            it.next();
        } else {
            panic!("lexer couldn't discern some symbol: {}", ch);
        }
    }
}

fn perform(s: &str) -> LexerCtx {
    let mut iter = s.char_indices().peekable();
    let mut ctx = LexerCtx::new(s.len());

    matcher(&mut iter, &mut ctx);

    ctx
}

pub fn lex(s: &str) -> Lexemes {
    let ctx = perform(s);
    let eof_span = ctx.eof_span();
    Lexemes::new(ctx.ret, eof_span)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_num(str: &str, num: u64) {
        let lexemes = lex(str);
        assert_eq!(lexemes.into_iter().count(), 1);

        let mut lexemes = lex(str);
        let token = lexemes.next().value.clone();
        match token {
            Token::Num(number) => assert_eq!(number, num),
            _ => assert!(false),
        }
    }

    #[test]
    fn test_overall() {
        let str = "134\n431\n138\n";
        let lexemes = lex(str);
        let expected = vec![
            Token::Num(134),
            Token::EOL,
            Token::Num(431),
            Token::EOL,
            Token::Num(138),
            Token::EOL,
        ];
        lexemes
            .into_iter()
            .map(|lexeme| lexeme.value)
            .zip(expected)
            .for_each(|(f, s)| assert_eq!(f, s));
    }
    #[test]
    fn test_num1() {
        test_num("1345", 1345);
    }
    #[test]
    fn test_num2() {
        test_num("140124", 140124);
    }
    #[test]
    fn test_num3() {
        test_num("0", 0);
    }
}
