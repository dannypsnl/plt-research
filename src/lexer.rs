#[derive(Debug, PartialEq)]
pub enum TkValue {
    Int(i32),
}

#[derive(Debug, PartialEq)]
pub struct Token((u32, u32), TkValue);

pub fn lex<'a>(code: &'a str) -> Vec<Token> {
    let mut tokens = Vec::<Token>::new();
    let mut chars = code.chars();
    loop {
        if let Some(c) = chars.next() {
            if c.is_digit(10) {
                let mut t = String::new();
                t.push(c);
                while let Some(c) = chars.next() {
                    if c.is_digit(10) {
                        t.push(c);
                    } else {
                        break;
                    }
                }
                if let Ok(i) = t.parse::<i32>() {
                    tokens.push(Token((0, 0), TkValue::Int(i)));
                }
            }
        } else {
            break;
        }
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
        assert_eq!(lex("1"), vec![Token((0, 0), TkValue::Int(1))]);
        assert_eq!(
            lex("1 2"),
            vec![
                Token((0, 0), TkValue::Int(1)),
                Token((0, 2), TkValue::Int(2)),
            ]
        );
    }
}
