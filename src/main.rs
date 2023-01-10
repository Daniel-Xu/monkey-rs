extern crate core;

mod ast;
mod evaluator;
mod lexer;
mod object;
mod parser;
mod token;

use crate::lexer::Lexer;
use crate::parser::Parser;
use std::io::{self, BufRead};
use std::mem::size_of;
fn main() {
    let stdin = io::stdin();
    // std error is unbuffered!!!
    eprint!(">>> ");
    for line in stdin.lock().lines() {
        let l = Lexer::new(line.unwrap());
        let mut p = Parser::new(l);
        let program = p.parse_program();

        p.check_parser_errors();
        println!("{:?}", program);
        eprint!(">>> ");
    }
}
