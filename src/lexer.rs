#[derive(Debug, PartialEq)]
enum TkValue {
    Int(i32),
}

#[derive(Debug, PartialEq)]
struct Token((u32, u32), TkValue);

struct Lexer {}

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
}
