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

// int, int*
fn parse_type(parser: &mut Parser) -> Result<Type, ParseError> {
    if parser.predict(vec![TkType::Ident]) != -1 {
        return Err(ParseError {});
    }
    if parser.predict(vec![TkType::Ident, TkType::Pointer]) == -1 {
        let r = Ok(Type(true, parser.take().value()));
        parser.drop();
        return r;
    }
    Ok(Type(false, parser.take().value()))
}
fn parse_parameters(parser: &mut Parser) -> Result<Vec<Parameter>, ParseError> {
    let mut params = vec![];
    loop {
        let t = parse_type(parser);
        if t.is_err() {
            return Err(t.unwrap_err());
        }
        if parser.predict(vec![TkType::Ident, TkType::Comma]) == -1 {
            params.push(Parameter(t.unwrap(), parser.take().value()));
            parser.drop();
        } else if parser.predict(vec![TkType::Ident, TkType::RParen]) == -1 {
            params.push(Parameter(t.unwrap(), parser.take().value()));
            parser.drop();
            break;
        } else {
            return Err(ParseError {});
        }
    }
    Ok(params)
}
// int add(int i, int j);
//
// int add(int i, int j) {
//   return i + j;
// }
fn parse_function(parser: &mut Parser) -> Result<Top, ParseError> {
    let typ = parse_type(parser);
    if typ.is_err() {
        return Err(typ.unwrap_err());
    }
    if parser.predict(vec![TkType::Ident, TkType::LParen]) != -1 {
        return Err(ParseError {});
    }
    let function_name = parser.take();
    parser.drop(); // drop Left Parent: (
    let params = parse_parameters(parser);
    if params.is_err() {
        return Err(params.unwrap_err());
    }
    Ok(Top::Func(
        typ.unwrap(),
        function_name.value(),
        params.unwrap(),
    ))
}

#[cfg(test)]
mod tests {
    use super::lexer::lex;
    use super::*;

    #[test]
    fn function_parse() {
        let p = &mut Parser::from(lex("int add(int x, int y)"));
        let r = parse_function(p);
        assert_eq!(
            r.unwrap(),
            Top::Func(
                Type(false, "int".to_string()),
                "add".to_string(),
                vec![
                    Parameter(Type(false, "int".to_string()), "x".to_string()),
                    Parameter(Type(false, "int".to_string()), "y".to_string()),
                ]
            )
        );
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
