// use crate::lexer::Lexer;
// use crate::parser::Parser;

use crate::token::Token;

// this is the expr
#[derive(Debug, PartialEq)]
pub enum Expr {
    Identifier(String),
    Integer(u32),
    Boolean(bool),
    Prefix(Token, Box<Expr>),           // token, right
    Infix(Box<Expr>, Token, Box<Expr>), //left, token, right // TODO: change this to struct
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Let(String, Expr),
    Return(Expr),
    Expression(Expr), // this is expression stmt
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub statements: Vec<Stmt>,
}

impl Program {
    pub fn new() -> Self {
        Self { statements: vec![] }
    }

    // fn from_input(input: &str) -> Self {
    //     // parser parser => program
    //     let lexer = Lexer::new(input.to_string());
    //     let mut parser = Parser::new(lexer);
    //     let program = parser.parse_statements();
    //
    //     program
    // }
}
