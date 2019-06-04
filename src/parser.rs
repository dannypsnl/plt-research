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
    /// new create Parser from code
    pub fn new(code: String) -> Parser {
        let tokens = lexer::lex(code);
        Parser {
            tokens: tokens,
            offset: 0,
        }
    }
    /// peek get the token by current add n
    pub fn peek(&self, n: usize) -> Result<Token> {
        self.get_token(self.offset + n)
    }
    /// consume take the token but don't use it
    pub fn consume(&mut self) -> Result<()> {
        self.take()?;
        Ok(())
    }
    /// take add current token position
    pub fn take(&mut self) -> Result<Token> {
        self.offset += 1;
        self.get_token(self.offset - 1)
    }
    fn get_token(&self, n: usize) -> Result<Token> {
        if self.tokens.len() <= n {
            Err(ParseError::new("eof".to_string()))
        } else {
            Ok(self.tokens[n].clone())
        }
    }

    fn matched(&self, token_type: &TkType, expected_type: &TkType) -> bool {
        *token_type == *expected_type
    }

    pub fn predict(&self, wants: Vec<TkType>) -> Result<()> {
        for (i, v) in wants.iter().enumerate() {
            let tk = self.peek(i)?;
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
    pub fn parse_type(&mut self) -> Result<Type> {
        self.predict(vec![TkType::Ident])?;
        let typ = self.take()?.value();

        let mut result = Type::Normal(typ);
        while self.predict(vec![TkType::Pointer]).is_ok() {
            self.consume()?;
            result = Type::Pointer(Box::new(result));
        }
        Ok(result)
    }
    /// parse_parameters
    ///
    /// format:
    /// ```
    /// type param (, type param)*
    /// ```
    pub fn parse_parameters(&mut self) -> Result<Vec<Parameter>> {
        let mut params = vec![];
        loop {
            let typ = self.parse_type()?;
            self.predict(vec![TkType::Ident])?;
            params.push(Parameter(typ, self.take()?.value()));
            if self.predict(vec![TkType::Comma]).is_ok() {
                // consume comma: ,
                self.consume()?;
            } else if self.predict(vec![TkType::RParen]).is_ok() {
                // consume right parent: )
                self.consume()?;
                return Ok(params);
            } else {
                return Err(ParseError::new(format!(
                    "expected comma or right paren but got unexpected: {:?}",
                    self.peek(0)?,
                )));
            }
        }
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
        let function_name = self.take()?;
        self.consume()?; // consume left parent: (
        let params = self.parse_parameters()?;
        self.predict(vec![TkType::Semicolon])?;
        Ok(Top::Func(typ, function_name.value(), params))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_function_declaration() {
        let mut p = Parser::new("int add(int x, int* y);".to_string());
        let r = p.parse_function().unwrap();
        assert_eq!(
            r,
            Top::Func(
                Type::Normal("int".to_string()),
                "add".to_string(),
                vec![
                    Parameter(Type::Normal("int".to_string()), "x".to_string()),
                    Parameter(
                        Type::Pointer(Box::new(Type::Normal("int".to_string()))),
                        "y".to_string()
                    ),
                ]
            )
        );
    }

    #[test]
    fn parse_type_format() {
        let mut p = Parser::new("int***".to_string());
        let r = p.parse_type().unwrap();
        assert_eq!(
            r,
            Type::Pointer(Box::new(Type::Pointer(Box::new(Type::Pointer(Box::new(
                Type::Normal("int".to_string())
            ))))))
        );
    }
}
