use super::ast::*;
use super::lexer::{TkType, Token};

use std::error::Error;
use std::fmt;

pub type Result<T> = std::result::Result<T, ParseError>;

#[derive(Debug)]
pub struct ParseError;
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

pub struct Parser {
    tokens: Vec<Token>,
    offset: usize,
}

// Parsing helper
impl Parser {
    pub fn from(tokens: Vec<Token>) -> Parser {
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

    /// parse_type
    ///
    /// format:
    /// ```
    /// int, int*
    /// ```
    fn parse_type(&mut self) -> Result<Type> {
        if self.predict(vec![TkType::Ident]) != -1 {
            return Err(ParseError {});
        }
        if self.predict(vec![TkType::Ident, TkType::Pointer]) == -1 {
            let r = Ok(Type(true, self.take().value()));
            self.drop();
            return r;
        }
        Ok(Type(false, self.take().value()))
    }
    /// parse_parameters
    ///
    /// format:
    /// ```
    /// type param (, type param)*
    /// ```
    fn parse_parameters(&mut self) -> Result<Vec<Parameter>> {
        let mut params = vec![];
        loop {
            let typ = self.parse_type()?;
            if self.predict(vec![TkType::Ident, TkType::Comma]) == -1 {
                params.push(Parameter(typ, self.take().value()));
                self.drop();
            } else if self.predict(vec![TkType::Ident, TkType::RParen]) == -1 {
                params.push(Parameter(typ, self.take().value()));
                self.drop();
                break;
            } else {
                return Err(ParseError {});
            }
        }
        Ok(params)
    }
    /// parse_function
    ///
    /// format:
    /// ```
    /// int add(int i, int j);
    ///
    /// int add(int i, int j) {
    ///   return i + j;
    /// }
    /// ```
    pub fn parse_function(&mut self) -> Result<Top> {
        let typ = self.parse_type()?;
        if self.predict(vec![TkType::Ident, TkType::LParen]) != -1 {
            return Err(ParseError {});
        }
        let function_name = self.take();
        self.drop(); // drop Left Parent: (
        let params = self.parse_parameters()?;
        Ok(Top::Func(typ, function_name.value(), params))
    }
}

#[cfg(test)]
mod tests {
    use super::super::lexer::lex;
    use super::*;

    #[test]
    fn function_parse() {
        let mut p = Parser::from(lex("int add(int x, int y)".to_string()));
        let r = p.parse_function();
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
