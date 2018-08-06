// Syntax:
// 6               # f64
// x = 3           # Assignment
// "hello"         # string
// <1, 2, 3>       # list
// ("Dog", 13)     # tuple
//
// add x y = x + y # function def
// [add: x, y]     # function call
// |x| {x+1}       # lambda

use super::lexer::{TkType, Token, lex};

struct Parser {
    tokens: Vec<Token>,
    offset: usize,
}

impl Parser {
    fn from(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens: tokens,
            offset: 0,
        }
    }

    fn peek(&self, n: usize) -> &Token {
        &self.tokens[self.offset+n]
    }

    fn program(&mut self) {
        let token = self.peek(0);
        while *token.tk_type() != TkType::EOF {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_parser() {
        let parser = Parser::from(lex("lalala"));
        let token = &parser.tokens[0];
        assert_eq!(*token.location(), (1, 0));
        assert_eq!(*token.value(), "lalala".to_string());
    }
}
