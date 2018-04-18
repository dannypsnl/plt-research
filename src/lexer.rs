#[derive(Debug, PartialEq)]
enum TkValue {
    Int(i32),
}

#[derive(Debug, PartialEq)]
struct Token((u32, u32), TkValue);

pub struct Lexer {
    source: String,
    result: Vec<Token>,
}

impl Lexer {
    fn from<'a>(code: &'a str) -> Lexer {
        Lexer {
            source: String::from(code),
            result: Vec::new(),
        }
    }
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
    fn create_lexer() {
        let lexer = Lexer::from("some code");
        assert_eq!(String::from("some code"), lexer.source);
    }
}
