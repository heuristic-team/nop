use std::iter::Peekable;
use std::process::exit;
use std::str::CharIndices;

// TODO: maybe unhardcode 2 spaces for block. Will have to think about it.

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Token {
    Id(String),
    Num(u64),
    ScopeStart,
    ScopeEnd,
}

#[derive(Debug, Clone, Copy)]
struct Location {
    line: usize,
    symbol: usize,
}

impl Location {
    fn new(line: usize, symbol: usize) -> Self {
        Location { line, symbol }
    }
}

#[derive(Debug, Clone, Copy)]
struct Span {
    start: Location,
    end: Location,
}

impl Span {
    fn new(start: Location, end: Location) -> Self {
        Span { start, end }
    }
}

#[derive(Debug, Clone)]
pub struct Lexeme {
    pub token: Token,
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
    fn new_line(&mut self) {
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
        //panic!("Tried to peek location on nothing"); idk if it's correct decision not to panic
        //there. feels right actually.
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

fn matcher<'a>(it: &mut It<'a>, ctx: &mut LexerCtx) {
    lex_linestart(it, ctx);
    while let Some(&(_, ch)) = it.peek() {
        if ch.is_numeric() {
            lex_number(it, ctx);
        }
        if ch == '\n' {
            it.next();
            lex_linestart(it, ctx);
            continue;
        }
        if ch.is_whitespace() {
            it.next();
            continue;
        }
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
    ctx.new_line();
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

mod tests {
    use super::*;
    #[warn(dead_code)]
    fn test_num(str: String, num: u64) {
        let lexems = lex(str);
        assert_eq!(lexems.len(), 1);
        let token = lexems[0].token.clone();
        match token {
            Token::Num(number) => assert_eq!(number, num),
            _ => assert!(false),
        }
    }
    #[warn(dead_code)]
    fn test_indent(str: String, indent: usize) {
        let lexems = lex(str);
        let mut indent_count = 0;
        for i in &lexems {
            if i.token != Token::ScopeStart {
                break;
            }
            indent_count += 1;
        }
        assert_eq!(indent, indent_count);
        let scope_starts = lexems
            .iter()
            .filter(|lexeme| lexeme.token == Token::ScopeStart)
            .count();
        let scope_ends = lexems
            .iter()
            .filter(|lexeme| lexeme.token == Token::ScopeEnd)
            .count();
        assert_eq!(scope_starts, scope_ends);
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
