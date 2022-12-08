// use crate::lexer::Lexer;
// use crate::parser::Parser;

#[derive(Debug, PartialEq)]
pub enum Expr {
    Identifier(String),
    Integer(usize),
    Boolean(bool),
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Let(String, Expr),
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
