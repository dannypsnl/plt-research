use super::ast::*;
use super::lexer;
use super::lexer::{TkType, Token};

use std::error::Error;
use std::fmt;

struct Parser {
    tokens: Vec<Token>,
    offset: usize,
}

// Parsing helper
impl Parser {
    fn from(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens: tokens,
            offset: 0,
        }
    }

    fn peek(&self, n: usize) -> &Token {
        &self.tokens[self.offset + n]
    }

    fn drop(&mut self) {
        self.take();
    }
    fn take(&mut self) -> Token {
        self.offset += 1;
        self.tokens[self.offset - 1].clone()
    }

    fn matched(&self, token_type: &TkType, expected_type: &TkType) -> bool {
        *token_type == *expected_type
    }

    fn predict(&self, wants: Vec<TkType>) -> i64 {
        for (i, v) in wants.iter().enumerate() {
            let tk = self.peek(i);
            if !self.matched(tk.tk_type(), v) {
                return i as i64;
            }
        }
        -1 // -1 means all matched
    }
}

// int add(int i, int j);
//
// int add(int i, int j) {
//   return i + j;
// }
fn parse_function(parser: &mut Parser) -> Result<Top, ParseError> {
    if parser.predict(vec![TkType::Ident, TkType::Ident, TkType::LParen]) != -1 {
        return Err(ParseError {});
    }
    let return_type = parser.take();
    let function_name = parser.take();
    parser.drop();
    Ok(Top::Func(return_type.value(), function_name.value()))
}

#[cfg(test)]
mod tests {
    use super::lexer::lex;
    use super::*;

    #[test]
    fn function_parse() {
        let p = &mut Parser::from(lex("int add()"));
        let r = parse_function(p);
        assert_eq!(r.unwrap(), Top::Func("int".to_string(), "add".to_string()));
    }
}

#[derive(Debug)]
struct ParseError;
impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ParseError")
    }
}
impl Error for ParseError {
    fn description(&self) -> &str {
        "parse error"
    }
}
