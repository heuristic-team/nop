use std::iter::Peekable;
use std::str::CharIndices;

// TODO: maybe unhardcode 2 spaces for block. Will have to think about it.

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Token {
    Id(String),
    Num(u64),
    Assign,
    Eq,
    Fn,
    Let,
    Type,
    EOL,
    ScopeStart,
    ScopeEnd,
    LParen,
    RParen,
    Quotes,
    Dot,
}

static KEYWORDS: &[(Token, &str)] = &[
    (Token::Fn, "fn"),
    (Token::Let, "let"),
    (Token::Type, "type"),
];

static SYMBOLS: &[(char, Token)] = &[
    ('(', Token::LParen),
    (')', Token::RParen),
    ('\"', Token::Quotes),
    ('.', Token::Dot),
];

#[derive(Debug, Clone, Copy)]
pub struct Location {
    pub line: usize,
    pub symbol: usize,
}

impl Location {
    fn new(line: usize, symbol: usize) -> Self {
        Location { line, symbol }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: Location,
    pub end: Location,
}

impl Span {
    fn new(start: Location, end: Location) -> Self {
        Span { start, end }
    }
}

#[derive(Debug, Clone)]
pub struct Lexeme {
    pub token: Token,
    pub span: Span,
}

impl Lexeme {
    fn new(token: Token, span: Span) -> Self {
        Lexeme { token, span }
    }
}

type It<'a> = Peekable<CharIndices<'a>>;

struct LexerCtx {
    line: usize,
    indent: usize,
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

    fn process_indent<'a>(&mut self, it: &mut It<'a>, indent: usize) {
        if indent > self.indent {
            let indent_diff = indent - self.indent;
            for _ in 0..indent_diff {
                let loc = self.current_location(it);
                let span = Span::new(loc, loc);
                let lexeme = Lexeme::new(Token::ScopeStart, span);
                self.add_lexeme(lexeme);
            }
        } else if indent < self.indent {
            let indent_diff = self.indent - indent;
            for _ in 0..indent_diff {
                let loc = self.current_location(it);
                let span = Span::new(loc, loc);
                let lexeme = Lexeme::new(Token::ScopeEnd, span);
                self.add_lexeme(lexeme);
            }
        }
        self.indent = indent;
    }

    fn close_indents(&mut self, file_size: usize) {
        for _ in 0..self.indent {
            let loc = Location::new(self.line, file_size);
            let span = Span::new(loc, loc);
            let lexeme = Lexeme::new(Token::ScopeEnd, span);
            self.add_lexeme(lexeme);
        }
    }

    fn new(file_size: usize) -> Self {
        LexerCtx {
            line: 0,
            indent: 0,
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

    if symbol == '=' {
        let token = multiple_symbol(it, "==", Token::Eq);

        if token.is_none() {
            it.next();
        }

        let token = token.unwrap_or(Token::Assign);

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
    lex_linestart(it, ctx);
    while let Some(&(_, ch)) = it.peek() {
        if let Some(lexeme) = lex_symbols(it, ctx) {
            ctx.add_lexeme(lexeme);
            continue;
        }
        if valid_id_start(ch) {
            let id = lex_id(it, ctx);
            ctx.add_lexeme(id);
            continue;
        }
        if ch.is_numeric() {
            lex_number(it, ctx);
            continue;
        }
        if ch == '\n' {
            it.next();
            ctx.new_line(it);
            lex_linestart(it, ctx);
            continue;
        }
        if ch.is_whitespace() {
            it.next();
            continue;
        }
        panic!("lexer couldn't discern some symbol: {}", ch);
    }
}

fn lex_indent<'a>(it: &mut It<'a>, ctx: &mut LexerCtx) -> usize {
    let mut indent_amount = 0;
    while let Some(&(_, ch)) = it.peek() {
        if ch == '\n' {
            return ctx.indent;
        }
        if !ch.is_whitespace() {
            break;
        }
        indent_amount += 1;
        it.next();
    }
    indent_amount / 2
}

fn lex_linestart<'a>(it: &mut It<'a>, ctx: &mut LexerCtx) {
    if !it.peek().is_none() {
        let indent = lex_indent(it, ctx);
        ctx.process_indent(it, indent);
    }
}

fn perform(s: String) -> LexerCtx {
    let mut iter = s.char_indices().peekable();
    let mut ctx = LexerCtx::new(s.len());

    matcher(&mut iter, &mut ctx);
    ctx.close_indents(s.len());

    ctx
}

pub fn lex(s: String) -> Vec<Lexeme> {
    perform(s).ret
}

#[allow(dead_code)]
mod tests {
    use super::*;
    fn test_num(str: String, num: u64) {
        let lexems = lex(str);
        assert_eq!(lexems.len(), 1);
        let token = lexems[0].token.clone();
        match token {
            Token::Num(number) => assert_eq!(number, num),
            _ => assert!(false),
        }
    }
    fn count_token(lexemes: &Vec<Lexeme>, token: Token) -> usize {
        lexemes.iter().filter(|t| t.token == token).count()
    }
    fn test_eq_indents(str: String) {
        let lexemes = lex(str);
        let scope_starts = count_token(&lexemes, Token::ScopeStart);
        let scope_ends = count_token(&lexemes, Token::ScopeEnd);
        assert_eq!(scope_starts, scope_ends);
    }

    fn test_indent(str: String, indent: usize) {
        let lexems = lex(str.clone());
        let mut indent_count = 0;
        for i in &lexems {
            if i.token != Token::ScopeStart {
                break;
            }
            indent_count += 1;
        }
        assert_eq!(indent, indent_count);
        test_eq_indents(str);
    }
    #[test]
    fn test_overall() {
        let str = "  134\n    431\n  138\n".to_string();
        let lexemes = lex(str);
        let expected = vec![
            Token::ScopeStart,
            Token::Num(134),
            Token::EOL,
            Token::ScopeStart,
            Token::Num(431),
            Token::EOL,
            Token::ScopeEnd,
            Token::Num(138),
            Token::EOL,
            Token::ScopeEnd,
        ];
        let tokens: Vec<_> = lexemes.into_iter().map(|lexeme| lexeme.token).collect();
        let _: Vec<_> = tokens
            .into_iter()
            .zip(expected)
            .map(|(f, s)| assert_eq!(f, s))
            .collect();
    }
    #[test]
    fn test_indent1() {
        test_indent("    145".to_string(), 2);
    }
    #[test]
    fn test_indent2() {
        test_indent("145".to_string(), 0);
    }
    #[test]
    fn test_indent3() {
        test_indent(" 145".to_string(), 0);
    }
    #[test]
    fn test_indent4() {
        test_indent("  145".to_string(), 1);
    }
    #[test]
    fn test_indent5() {
        test_indent(
            "  145\
                 148"
            .to_string(),
            1,
        );
    }
    #[test]
    fn test_num1() {
        test_num("1345".to_string(), 1345);
    }
    #[test]
    fn test_num2() {
        test_num("140124".to_string(), 140124);
    }
    #[test]
    fn test_num3() {
        test_num("0".to_string(), 0);
    }
}
