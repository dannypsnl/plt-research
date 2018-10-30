mod ast;
mod lexer;
mod parser;

fn main() {
    let tokens = lexer::lex("abc");
    // let parser = parser::Parser::new();
}
