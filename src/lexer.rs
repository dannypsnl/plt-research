#[derive(Debug, PartialEq)]
pub enum TkValue {
    EOF,
    Int(i32),
}

#[derive(Debug, PartialEq)]
pub struct Token((i32, i32), TkValue);

struct lexer<'a> {
    code: &'a str,
    pos: u32,
    line: u32,
    index: usize,
}

impl<'a> lexer<'a> {
    fn new(code: &'a str) -> lexer {
        lexer {
            code: code,
            pos: 0,
            line: 0,
            index: 0,
        }
    }

    fn next_char(&mut self) -> char {
        self.index += 1;
        self.code.next()
    }

    fn next(&mut self) -> Token {
        Token((0, 0), TkValue::EOF)
    }
}

pub fn lex<'a>(code: &'a str) -> Vec<Token> {
    let mut tokens = Vec::<Token>::new();
    let mut lexer = lexer::new(code);

    loop {
        let tk = lexer.next();
        match tk {
            Token(_, EOF) => {
                break;
            }
        }
        tokens.push(tk);
    }

    tokens
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn compare_tkvalue() {
        assert_eq!(TkValue::Int(10), TkValue::Int(10));
    }

    #[test]
    fn compare_token() {
        assert_eq!(
            Token((0, 0), TkValue::Int(3)),
            Token((0, 0), TkValue::Int(3))
        );
    }

    #[test]
    fn lex_return_tokens() {
        assert_eq!(lex("1"), vec![Token((1, 0), TkValue::Int(1))]);
        assert_eq!(
            lex("1 2"),
            vec![
                Token((1, 0), TkValue::Int(1)),
                Token((1, 2), TkValue::Int(2)),
            ]
        );
    }
}
