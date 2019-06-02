mod ast;
mod lexer;
mod parser;

use std::env::args;
use std::fs;

fn main() {
    let code = fs::read_to_string(args().nth(1).unwrap()).unwrap();
    let tokens = lexer::lex(code);
    let _parser = parser::Parser::from(tokens);
}
