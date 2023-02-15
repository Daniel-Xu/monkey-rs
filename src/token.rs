use std::fmt::{Display, Formatter, Write};

#[derive(PartialEq, Debug, Eq, Clone)]
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
    Int(i32),
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
    LParen, // (
    RParen,
    LBrace, // {
    RBrace,
    LBracket, // [
    RBracket,
    Eof,
    Illegal(char),
    Bang,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Let => f.write_str("let"),
            Token::Function => f.write_str("fn"),
            Token::If => f.write_str("if"),
            Token::Return => f.write_str("return"),
            Token::True => f.write_str("true"),
            Token::False => f.write_str("false"),
            Token::Else => f.write_str("else"),

            Token::Ident(s) => f.write_str(s),
            Token::Int(i) => f.write_str(&i.to_string()),
            Token::Str(s) => f.write_str(s),

            // delimiters
            Token::Semicolon => f.write_str(";"),
            Token::Comma => f.write_str(","),
            Token::Colon => f.write_str(":"),

            // operators which have precedence
            Token::Plus => f.write_str("+"),
            Token::Minus => f.write_str("-"),
            Token::Asterisk => f.write_str("*"),
            Token::Slash => f.write_str("/"),
            Token::Lt => f.write_str("<"),
            Token::Gt => f.write_str(">"),
            Token::Eq => f.write_str("=="),
            Token::NotEq => f.write_str("!="),

            // special char
            Token::Assign => f.write_str("="),
            Token::LParen => f.write_str("("),
            Token::RParen => f.write_str(")"),
            Token::LBrace => f.write_str("{"),
            Token::RBrace => f.write_str("}"),
            Token::LBracket => f.write_str("["),
            Token::RBracket => f.write_str("]"),
            Token::Eof => f.write_str("\0"),
            Token::Illegal(c) => f.write_str(&c.to_string()),
            Token::Bang => f.write_str("!"),
        }
    }
}
