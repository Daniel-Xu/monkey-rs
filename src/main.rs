mod ast;
mod lexer;
mod parser;
mod token;
// use clap::Parser;

// #[derive(Parser, Debug)]
// struct Args {
//     /// Name of the person to greet
//     #[arg(short, long)]
//     name: String,
//
//     /// Number of times to greet
//     #[arg(short, long, default_value_t = 1)]
//     count: u8,
// }

use crate::lexer::Lexer;
use std::io::{self, BufRead};

fn main() {
    let stdin = io::stdin();
    // std error is unbuffered!!!
    eprint!(">>> ");
    for line in stdin.lock().lines() {
        let l = Lexer::new(line.unwrap());
        for element in l {
            println!("the value is: {:?}", element);
        }
        eprint!(">>> ");
    }
}
