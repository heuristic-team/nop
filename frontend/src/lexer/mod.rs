use std::iter::{Enumerate, Peekable};
use std::str::Chars;

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
    (Token::For, "for"),
    (Token::Do, "do"),
    (Token::If, "if"),
    (Token::Then, "then"),
    (Token::Else, "else"),
    (Token::Struct, "struct"),
];

/// Single-char tokens
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

/// Pairs of symbols where the first is a prefix of the second, like `=` and `==`.
const MULTIPLE_CHAR_SYMBOLS: &[(char, Token, &'static str, Token)] = &[
    (':', Token::Colon, ":=", Token::Define),
    ('-', Token::Minus, "->", Token::Arrow),
    ('=', Token::Assign, "==", Token::Eq),
    ('!', Token::Exclam, "!=", Token::NotEq),
    ('<', Token::Less, "<=", Token::LessEq),
    ('>', Token::Greater, ">=", Token::GreaterEq),
    ('&', Token::Amper, "&&", Token::And),
    ('|', Token::Vbar, "||", Token::Or),
];

fn valid_id_start(ch: char) -> bool {
    ch.is_alphabetic() || ch == '_'
}

fn valid_id(ch: char) -> bool {
    ch.is_alphanumeric() || ch == '_'
}

/// Character iterator used for lexing
type It<'a> = Peekable<Enumerate<Chars<'a>>>;

struct Lexer<'a> {
    it: It<'a>,
    file_size: usize,
    ret: Vec<Lexeme>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            file_size: input.len(),
            ret: Vec::new(),
            it: input.chars().enumerate().peekable(),
        }
    }

    pub fn run(mut self) -> Lexemes {
        self.match_loop();
        let eof_span = Span::new(self.file_size, self.file_size);
        Lexemes::new(self.ret, eof_span)
    }

    fn current_offset(&mut self) -> usize {
        self.it.peek().map(|&(n, _)| n).unwrap_or(self.file_size)
    }

    fn new_line(&mut self) {
        let pos = self.current_offset() - 1;
        let token = Token::EOL;
        let span = Span::new(pos, pos);
        let lexeme = Lexeme::new(token, span);
        self.add_lexeme(lexeme);
    }

    fn add_lexeme(&mut self, lexeme: Lexeme) {
        self.ret.push(lexeme);
    }

    fn eat_while(&mut self, mut f: impl FnMut(char) -> bool) -> String {
        let mut ret = String::new();

        while let Some(&(_, ch)) = self.it.peek() {
            if !f(ch) {
                break;
            }
            ret.push(ch);
            self.it.next();
        }

        ret
    }

    fn lex_number(&mut self) {
        let start = self.current_offset();
        let num = self.eat_while(char::is_numeric);
        let end = self.current_offset() - 1;

        let span = Span::new(start, end);
        let token = Token::Num(num.parse().unwrap());
        let lexeme = Lexeme::new(token, span);
        self.add_lexeme(lexeme);
    }

    fn multiple_symbol(&mut self, expected: &str, expected_token: Token) -> Option<Token> {
        let mut cloned = self.it.clone();

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
            self.it.next();
        }

        Some(expected_token)
    }

    fn single_symbol(&mut self) -> Option<Lexeme> {
        let &(_, symbol) = self.it.peek().unwrap();

        for (ch, token) in SYMBOLS {
            if *ch == symbol {
                let start = self.current_offset();
                self.it.next();

                let span = Span::new(start, start);

                let lexeme = Lexeme::new(token.clone(), span);

                return Some(lexeme);
            }
        }
        None
    }

    fn lex_symbols(&mut self) -> Option<Lexeme> {
        let try_single = self.single_symbol();

        if try_single.is_some() {
            return try_single;
        }

        let start = self.current_offset();
        let &(_, symbol) = self.it.peek().unwrap();

        if let Some((fallback_token, long_token_literal, long_token)) = MULTIPLE_CHAR_SYMBOLS
            .iter()
            .find(|&(c, _, _, _)| *c == symbol)
            .map(|(_, t1, lit, t2)| (t1, lit, t2))
        {
            let token = self.multiple_symbol(long_token_literal, long_token.clone());
            if token.is_none() {
                self.it.next();
            }

            let token = token.unwrap_or(fallback_token.clone());
            let end = self.current_offset() - 1;
            let span = Span::new(start, end);
            let lexeme = Lexeme::new(token, span);

            return Some(lexeme);
        }

        None
    }

    fn lex_id(&mut self) -> Lexeme {
        let start = self.current_offset();
        let id = self.eat_while(valid_id);
        let end = self.current_offset() - 1;

        let span = Span::new(start, end);

        for (token, str) in KEYWORDS {
            if *str == id {
                return Lexeme::new(token.clone(), span);
            }
        }

        Lexeme::new(Token::Id(id), span)
    }

    fn match_loop(&mut self) {
        while let Some(&(_, ch)) = self.it.peek() {
            if let Some(lexeme) = self.lex_symbols() {
                self.add_lexeme(lexeme);
            } else if valid_id_start(ch) {
                let id = self.lex_id();
                self.add_lexeme(id);
            } else if ch.is_numeric() {
                self.lex_number();
            } else if ch == '\n' {
                self.it.next();
                self.new_line();
            } else if ch.is_whitespace() {
                self.it.next();
            } else {
                panic!("lexer couldn't discern some symbol: {}", ch);
            }
        }
    }
}

pub fn lex(s: &str) -> Lexemes {
    Lexer::new(s).run()
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
