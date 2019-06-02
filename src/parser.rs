use super::ast::*;
use super::lexer;
use super::lexer::{TkType, Token};

pub type Result<T> = std::result::Result<T, ParseError>;

#[derive(Debug)]
pub struct ParseError {
    descript: String,
}

impl ParseError {
    fn new(descript: String) -> ParseError {
        ParseError { descript: descript }
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "ParseError: {}", self.descript)
    }
}
impl std::error::Error for ParseError {
    fn description(&self) -> &str {
        "parse error"
    }
}

/// Parser is a parsing helper
pub struct Parser {
    tokens: Vec<Token>,
    offset: usize,
}

impl Parser {
    pub fn new(code: String) -> Parser {
        let tokens = lexer::lex(code);
        Parser::from(tokens)
    }
    fn from(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens: tokens,
            offset: 0,
        }
    }
    /// peek get the token by current add n
    fn peek(&self, n: usize) -> &Token {
        &self.tokens[self.offset + n]
    }
    /// consume take the token but don't use it
    fn consume(&mut self) {
        self.take();
    }
    /// take add current token position
    fn take(&mut self) -> Token {
        self.offset += 1;
        self.tokens[self.offset - 1].clone()
    }

    fn matched(&self, token_type: &TkType, expected_type: &TkType) -> bool {
        *token_type == *expected_type
    }

    fn predict(&self, wants: Vec<TkType>) -> Result<()> {
        for (i, v) in wants.iter().enumerate() {
            let tk = self.peek(i);
            if !self.matched(tk.tk_type(), v) {
                return Err(ParseError::new(format!(
                    "expected: {:?} but got {:?} at {:?}",
                    v,
                    tk.tk_type(),
                    tk.location(),
                )));
            }
        }
        Ok(())
    }
    /// parse_type
    ///
    /// format:
    /// ```
    /// int
    /// int*
    /// ```
    fn parse_type(&mut self) -> Result<Type> {
        self.predict(vec![TkType::Ident])?;
        let typ = self.take().value();
        if self.predict(vec![TkType::Pointer]).is_ok() {
            // consume pointer: *
            self.consume();
            Ok(Type(true, typ))
        } else {
            Ok(Type(false, typ))
        }
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
            self.predict(vec![TkType::Ident])?;
            params.push(Parameter(typ, self.take().value()));
            if self.predict(vec![TkType::Comma]).is_ok() {
                // consume comma: ,
                self.consume();
                continue;
            } else if self.predict(vec![TkType::RParen]).is_ok() {
                // consume right parent: )
                self.consume();
                break;
            } else {
                return Err(ParseError::new(format!(
                    "expected comma or right paren but got unexpected: {:?}",
                    self.peek(0),
                )));
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
        self.predict(vec![TkType::Ident, TkType::LParen])?;
        let function_name = self.take();
        self.consume(); // consume left parent: (
        let params = self.parse_parameters()?;
        Ok(Top::Func(typ, function_name.value(), params))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn function_parse() {
        let mut p = Parser::new("int add(int x, int* y)".to_string());
        let r = p.parse_function();
        assert_eq!(
            r.unwrap(),
            Top::Func(
                Type(false, "int".to_string()),
                "add".to_string(),
                vec![
                    Parameter(Type(false, "int".to_string()), "x".to_string()),
                    Parameter(Type(true, "int".to_string()), "y".to_string()),
                ]
            )
        );
    }
}
