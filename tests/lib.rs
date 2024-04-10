use dyck::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum TestToken {
    SquareOpen,
    SquareClose,
    ParenOpen,
    ParenClose,
    CurlyOpen,
    CurlyClose,
}

impl TestToken {
    fn from_char(c: char) -> Option<Self> {
        match c {
            '[' => Some(TestToken::SquareOpen),
            ']' => Some(TestToken::SquareClose),
            '(' => Some(TestToken::ParenOpen),
            ')' => Some(TestToken::ParenClose),
            '{' => Some(TestToken::CurlyOpen),
            '}' => Some(TestToken::CurlyClose),
            _ => None,
        }
    }
}

fn create_test_context() -> dyck::DyckContext {
    let pairs = vec![
        (TestToken::SquareOpen, TestToken::SquareClose),
        (TestToken::ParenOpen, TestToken::ParenClose),
        (TestToken::CurlyOpen, TestToken::CurlyClose),
    ];
    DyckContext::new_from_vec(&pairs).unwrap()
}

#[test]
fn test_new_from_vec() {
    let pairs = vec![
        (TestToken::SquareOpen, TestToken::SquareClose),
        (TestToken::ParenOpen, TestToken::ParenClose),
        (TestToken::CurlyOpen, TestToken::CurlyClose),
    ];
    let context = DyckContext::new_from_vec(&pairs).unwrap();
    assert_eq!(context.alphabet.len(), 6);
    assert_eq!(context.open_to_close.len(), 3);
    assert_eq!(context.close_to_open.len(), 3);
}
