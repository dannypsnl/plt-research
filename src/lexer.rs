#[derive(Debug, PartialEq)]
pub enum TkValue {
    EOF,
    Num(f64),
}

#[derive(Debug, PartialEq)]
pub struct Token((u32, u32), TkValue);

use std::str;

struct lexer<'a> {
    code: str::Chars<'a>,
    pos: u32,
    line: u32,
}

impl<'a> lexer<'a> {
    fn new(code: &'a str) -> lexer {
        lexer {
            code: code.chars(),
            pos: 0,
            line: 0,
        }
    }

    fn next_char(&mut self) -> char {
        self.pos += 1;
        let c = self.code.next();
        if let Some(c) = c {
            if c == '\n' {
                self.line += 1;
            }
            c
        } else {
            '\0'
        }
    }
    fn new_token(&mut self, typ: TkValue) -> Token {
        Token((self.pos, self.line), typ)
    }

    fn next(&mut self) -> Token {
        let c = self.next_char();
        if c == '\0' {
            self.new_token(TkValue::EOF)
        } else if c.is_digit(10) {
            println!("number");
            self.new_token(TkValue::Num(10.0))
        } else {
            self.new_token(TkValue::EOF)
        }
    }
}

pub fn lex<'a>(code: &'a str) -> Vec<Token> {
    let mut tokens = Vec::<Token>::new();
    let mut lexer = lexer::new(code);

    loop {
        let tk = lexer.next();
        match tk {
            Token(_, TkValue::EOF) => {
                break;
            }
            _ => (),
        }
        tokens.push(tk);
    }

    tokens
}

#[cfg(test)]
mod tests {
    use super::*;
    use self::TkValue::Num;

    #[test]
    fn compare_tkvalue() {
        assert_eq!(Num(10.0), Num(10.0));
    }

    #[test]
    fn compare_token() {
        assert_eq!(Token((0, 0), Num(3.0)), Token((0, 0), Num(3.0)));
    }

    #[test]
    fn lex_return_tokens() {
        assert_eq!(lex("1"), vec![Token((1, 0), Num(1.0))]);
        assert_eq!(
            lex("1 2"),
            vec![Token((1, 0), Num(1.0)), Token((1, 2), Num(2.0))]
        );
    }
}
