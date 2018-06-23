mod lexer;
mod parser;

fn main() {
    let res = lexer::lex("abc");
    println!("Lex result: {:?}", res);
    // let parser = parser::Parser::new();
}
