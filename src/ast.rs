// use crate::lexer::Lexer;
// use crate::parser::Parser;

use crate::token::Token;
use std::fmt::{Display, Formatter};

// this is the expr
#[derive(Debug, PartialEq)]
pub enum Expr {
    Identifier(String),
    Integer(u32),
    Boolean(bool),
    Prefix(Token, Box<Expr>),                    // token, right
    Infix(Box<Expr>, Token, Box<Expr>),          //left, token, right // TODO: change this to struct
    If(Box<Expr>, BlockStmt, Option<BlockStmt>), // condition blocks blocks
    Function(Vec<Expr>, BlockStmt),              // parameter, blocks
    Call(Box<Expr>, Vec<Expr>),                  //identifer, parameter
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

#[derive(Debug, PartialEq)]
pub struct BlockStmt {
    pub statements: Vec<Stmt>,
}

impl BlockStmt {
    pub fn new() -> Self {
        Self { statements: vec![] }
    }
    pub fn is_empty(&self) -> bool {
        self.statements.is_empty()
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
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
