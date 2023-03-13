extern crate core;

mod ast;
mod evaluator;
mod lexer;
mod object;
mod parser;
mod token;

use crate::ast::Program;
use crate::evaluator::eval;
use crate::object::environment::Environment;
use crate::object::NULL;
use std::io::{self, BufRead, Write};
use std::rc::Rc;

fn main() {
    let stdin = io::stdin();
    // std error is unbuffered!!!
    eprint!(">>> ");

    let env = Environment::new();
    for line in stdin.lock().lines() {
        let program = Program::from_input(&line.unwrap());

        // if there's no data to output, we will wait for the input
        // let x = 1;
        // x
        let res = match eval(program, Rc::clone(&env)) {
            Ok(o) if o == NULL => continue,
            Ok(o) => o.to_string(),
            Err(e) => e.to_string(),
        };

        eprintln!("{}", res);
        eprint!(">>> ");
    }
}
