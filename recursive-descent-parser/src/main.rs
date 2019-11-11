pub mod ast;
pub mod lexer;
pub mod parser;

use std::env::args;
use std::fs;

fn main() {
    let code = fs::read_to_string(args().nth(1).unwrap()).unwrap();
    let mut parser = parser::Parser::new(code);
    parser.parse_function().unwrap();
}
