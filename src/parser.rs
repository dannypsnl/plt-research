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

    fn matched(&self, token_type: &TkType, expected_type: TkType) -> bool {
        *token_type == expected_type
    }
}

struct AstTree {
    rules: Vec<Binding>,
}
// AST emit helper
impl AstTree {
    fn new() -> AstTree {
        AstTree { rules: Vec::new() }
    }
    fn add_binding(&mut self, b: Binding) {
        self.rules.push(b);
    }
}

struct Binding((u32, u32), String, Num);
struct Num((u32, u32), String);
// ident = expression
// TODO: using Num now instead of expression, because haven't implement it
fn binding(parser: &Parser, astTree: &mut AstTree) {
    let token1 = parser.peek(0);
    let r0 = parser.matched(token1.tk_type(), TkType::Ident);
    let token2 = parser.peek(1);
    let r1 = parser.matched(token2.tk_type(), TkType::Match);
    let token3 = parser.peek(2);
    let r2 = parser.matched(token3.tk_type(), TkType::Num);

    if r0 && r1 && r2 {
        astTree.add_binding(Binding(
            token1.location(),
            token1.value(),
            Num(token3.location(), token3.value()),
        ));
    }
}

#[cfg(test)]
mod tests {
    use super::lexer::lex;
    use super::*;

    #[test]
    fn new_parser() {
        let mut tree = AstTree::new();
        let parser = Parser::from(lex("a = 1"));
        binding(&parser, &mut tree);
        assert_eq!(tree.rules.len(), 1);
    }
}
