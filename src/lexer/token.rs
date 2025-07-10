use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Token {
    EOF,
    Id(String),
    Num(u64),
    Assign,
    Define,
    Eq,
    Fn,
    Type,
    Mut,
    EOL,
    ScopeStart,
    ScopeEnd,
    LParen,
    RParen,
    Quote,
    Dot,
    Comma,
    Colon,
    Plus,
    Minus,
    Mul,
}

impl Token {
    pub fn to_str(&self) -> &'static str {
        match self {
            Token::EOF => "end of input",
            Token::Id(_) => "identifier",
            Token::Num(_) => "number",
            Token::Assign => "`=`",
            Token::Define => "`:=`",
            Token::Eq => "`==`",
            Token::Fn => "`fn`",
            Token::Type => "`type`",
            Token::Mut => "`mut`",
            Token::EOL => "end of line",
            Token::ScopeStart => "scope start",
            Token::ScopeEnd => "scope end",
            Token::LParen => "`(`",
            Token::RParen => "`)`",
            Token::Quote => "`\"`",
            Token::Dot => "`.`",
            Token::Comma => "`,`",
            Token::Colon => "`:`",
            Token::Plus => "`+`",
            Token::Minus => "`-`",
            Token::Mul => "`*`",
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.to_str())
    }
}
