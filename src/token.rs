#[derive(PartialEq, Debug)]
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

    // operator
    Assign,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Lt,
    Gt,
    Eq,
    NotEq,

    // special char
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Eof,
    Illegal,
    Bang,
}
