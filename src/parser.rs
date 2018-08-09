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

use super::lexer;
use super::lexer::{TkType, Token};

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
        &self.tokens[self.offset + n]
    }

    fn matched(&self, token_type: &TkType, expected_type: TkType) -> bool {
        *token_type == expected_type
    }
}

// ident = expression
// TODO: using Num now instead of expression, because haven't implement it
fn binding(parser: &mut Parser) {
    let token = parser.peek(0);
    parser.matched(token.tk_type(), TkType::Ident);
    let token = parser.peek(1);
    parser.matched(token.tk_type(), TkType::Match);
    let token = parser.peek(2);
    parser.matched(token.tk_type(), TkType::Num);
}

#[cfg(test)]
mod tests {
    use super::lexer::lex;
    use super::*;

    #[test]
    fn new_parser() {
        let parser = Parser::from(lex("lalala"));
        let token = &parser.tokens[0];
        assert_eq!(*token.location(), (1, 0));
        assert_eq!(*token.value(), "lalala".to_string());
    }
}
