use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Token {
    EOF,
    Id(String),
    Num(u64),
    Assign,    // `=`
    Define,    // `:=`
    Eq,        // `==`
    NotEq,     // `!=`
    And,       // `&&`
    Or,        // `||`
    Less,      // `<`
    LessEq,    // `<=`
    Greater,   // `>`
    GreaterEq, // `>=`
    Arrow,     // `->`
    Fn,        // `fn`
    Type,      // `type`
    Mut,       // `mut`
    Ret,       // `ret`
    True,      // `true`
    False,     // `false`
    For,       // `for`
    Do,        // `do`
    If,        // `if`
    Then,      // `then`
    Else,      // `else`
    Struct,    // `struct`
    EOL,       //
    LParen,    // `(`
    RParen,    // `)`
    LBrace,    // `{`
    RBrace,    // `}`
    Quote,     // `"`
    Dot,       // `.`
    Comma,     // `,`
    Colon,     // `:`
    Plus,      // `+`
    Minus,     // `-`
    Mul,       // `*`
    Exclam,    // `!`
    Amper,     // `&`
    Vbar,      // `|`
}

impl Token {
    pub fn to_str(&self) -> &'static str {
        match self {
            Token::EOF => "end of input",
            Token::Id(_) => "identifier",
            Token::Num(_) => "number",
            Token::Assign => "`=`",
            Token::Define => "`:=`",
            Token::Arrow => "`->`",
            Token::Eq => "`==`",
            Token::NotEq => "`!=`",
            Token::And => "`&&`",
            Token::Or => "`||`",
            Token::Less => "`<`",
            Token::LessEq => "`<=`",
            Token::Greater => "`>`",
            Token::GreaterEq => "`>=`",
            Token::Fn => "`fn`",
            Token::Type => "`type`",
            Token::Mut => "`mut`",
            Token::Ret => "`ret`",
            Token::True => "`true`",
            Token::False => "`false`",
            Token::For => "`for`",
            Token::Do => "`do`",
            Token::If => "`if`",
            Token::Then => "`then`",
            Token::Else => "`else`",
            Token::Struct => "`struct`",
            Token::EOL => "end of line",
            Token::LParen => "`(`",
            Token::RParen => "`)`",
            Token::LBrace => "`{`",
            Token::RBrace => "`}`",
            Token::Quote => "`\"`",
            Token::Dot => "`.`",
            Token::Comma => "`,`",
            Token::Colon => "`:`",
            Token::Plus => "`+`",
            Token::Minus => "`-`",
            Token::Mul => "`*`",
            Token::Exclam => "`!`",
            Token::Amper => "`&`",
            Token::Vbar => "`|`",
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.to_str())
    }
}
