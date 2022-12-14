#[derive(PartialEq, Debug, Clone)]
pub enum Token {
    // keyword
    Let,
    Function,
    If,
    Return,
    True,
    False,
    Else,

    // id
    Ident(String),
    Int(u32),
    Str(String),

    // delimiters
    Semicolon,
    Comma,
    Colon,

    // operators which have precedence
    Plus,
    Minus,
    Asterisk,
    Slash,
    Lt,
    Gt,
    Eq,
    NotEq,

    // special char
    Assign,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Eof,
    Illegal(char),
    Bang,
}
