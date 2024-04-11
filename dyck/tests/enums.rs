#[cfg(feature = "derive")]
use dyck::{DyckToken, Language, Word};

#[cfg(feature = "derive")]
#[derive(DyckToken)]
enum Token {
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    OpenBrace,
    CloseBrace,
}

#[cfg(test)]
#[cfg(feature = "derive")]
mod core_tests {
    use super::*;

    #[test]
    fn new_from_vec_no_duplicates() {
        let pairs = vec![
            (Token::OpenParen, Token::CloseParen),
            (Token::OpenBracket, Token::CloseBracket),
        ];
        let language = Language::new_from_vec(&pairs).expect("failed to create language");
        assert_eq!(language.get_k(), 2);
    }

    #[test]
    fn new_from_arr_no_duplicates() {
        let pairs = [
            (Token::OpenParen, Token::CloseParen),
            (Token::OpenBracket, Token::CloseBracket),
        ];
        let language = Language::new_from_arr(&pairs).expect("failed to create language");
        assert_eq!(language.get_k(), 2);
    }

    #[test]
    fn is_open() {
        let pairs = vec![(Token::OpenBrace, Token::CloseBrace)];
        let language = Language::new_from_vec(&pairs).unwrap();
        assert!(language.is_open(Token::OpenBrace));
        assert!(!language.is_open(Token::CloseBrace));
    }

    #[test]
    fn is_close() {
        let pairs = vec![(Token::OpenBrace, Token::CloseBrace)];
        let language = Language::new_from_vec(&pairs).unwrap();
        assert!(language.is_close(Token::CloseBrace));
        assert!(!language.is_close(Token::OpenBrace));
    }

    #[test]
    fn is_balanced() {
        let pairs = vec![(Token::OpenBracket, Token::CloseBracket)];
        let language = Language::new_from_vec(&pairs).unwrap();
        let balanced_word: Word<Token> = vec![Token::OpenBracket, Token::CloseBracket];
        assert!(language.is_balanced(&balanced_word));
        let unbalanced_word: Word<Token> = vec![Token::OpenBracket, Token::OpenBracket];
        assert!(!language.is_balanced(&unbalanced_word));
    }
}
