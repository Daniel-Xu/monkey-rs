use crate::lexer::Lexer;
use crate::parser::Parser;

use crate::token::Token;
use std::fmt::{Display, Formatter};

// this is the expr
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    Identifier(String),
    Integer(i32),
    Boolean(bool),
    Str(String),
    Prefix(Token, Box<Expr>),                    // token, right
    Infix(Box<Expr>, Token, Box<Expr>),          //left, token, right // TODO: change this to struct
    If(Box<Expr>, BlockStmt, Option<BlockStmt>), // condition blocks blocks
    Function(Vec<Expr>, BlockStmt),              // parameter, blocks
    Call(Box<Expr>, Vec<Expr>),                  //identifer, parameter x(a, b, c)
    Array(Vec<Expr>),                            // len, element type
    Index(Box<Expr>, Box<Expr>),                 // identifier + [ + expr
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Identifier(s) => write!(f, "{}", s),
            Expr::Integer(v) => write!(f, "{}", v),
            Expr::Boolean(b) => write!(f, "{}", b),
            Expr::Str(s) => write!(f, "{}", s),
            Expr::Prefix(token, expr) => write!(f, "({}{})", token.to_string(), expr.to_string()),
            Expr::Infix(left, op, right) => write!(f, "({} {} {})", left, op, right),
            Expr::If(condition, if_block, else_block) => match else_block {
                Some(v) => write!(f, "if {} {} else {}", condition, if_block, v),
                None => write!(f, "if {} {}", condition, if_block),
            },
            Expr::Function(parameter, block) => {
                write!(f, "fn({}) {}", pretty_print(parameter), block)
            }
            Expr::Call(name, parameter) => write!(f, "{}({})", name, pretty_print(parameter)),
            Expr::Array(content) => write!(f, "[{}]", pretty_print(content)),
            Expr::Index(id, subscription) => {
                write!(f, "({}[{}])", id.to_string(), subscription.to_string())
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Stmt {
    Let(String, Expr),
    Return(Expr),
    Expression(Expr), // this is expression stmt
}

impl Display for Stmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Let(id, expr) => write!(f, "let {} = {};", id, expr),
            Stmt::Return(expr) => write!(f, "return {};", expr),
            Stmt::Expression(expr) => write!(f, "{}", expr),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub statements: Vec<Stmt>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
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

impl Display for BlockStmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s: String = self.statements.iter().map(|s| s.to_string()).collect();
        f.write_str(&s)
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = self
            .statements
            .iter()
            .map(|e| e.to_string())
            .collect::<Vec<String>>()
            .join("");

        write!(f, "{}", s)
    }
}

impl Program {
    pub fn new() -> Self {
        Self { statements: vec![] }
    }

    pub fn from_input(input: &str) -> Self {
        // parser parser => program
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        program
    }
}

pub fn pretty_print<T: Display>(content: &[T]) -> String {
    content
        .iter()
        .map(|e| e.to_string())
        .collect::<Vec<String>>()
        .join(", ")
}
