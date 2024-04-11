use dyck::{Language, Word};

#[cfg(test)]
mod dyck_core_tests {
    use super::*;

    #[test]
    fn test_new_from_vec_no_duplicates() {
        let pairs = vec![("(", ")"), ("[", "]")];
        let language = Language::new_from_vec(&pairs).expect("Failed to create language");
        assert_eq!(language.get_k(), 2);
    }

    #[test]
    fn test_new_from_vec_with_duplicate_opening_tokens() {
        let pairs = vec![("(", ")"), ("(", "]")];
        assert!(
            Language::new_from_vec(&pairs).is_err(),
            "Constructor should fail with duplicate opening tokens"
        );
    }

    #[test]
    fn test_new_from_vec_with_duplicate_closing_tokens() {
        let pairs = vec![("(", ")"), ("{", ")")];
        assert!(
            Language::new_from_vec(&pairs).is_err(),
            "Constructor should fail with duplicate closing tokens"
        );
    }

    #[test]
    fn test_new_from_vec_with_duplicate_pairs() {
        let pairs = vec![("(", ")"), ("(", ")")];
        assert!(
            Language::new_from_vec(&pairs).is_err(),
            "Constructor should fail with duplicate pairs"
        );
    }

    #[test]
    fn test_get_k_with_single_pair() {
        let pairs = vec![("(", ")")];
        let language =
            Language::new_from_vec(&pairs).expect("Failed to create language with single pair");
        assert_eq!(
            language.get_k(),
            1,
            "get_k should return 1 for a single pair of parentheses"
        );
    }

    #[test]
    fn test_get_k_with_multiple_pairs() {
        let pairs = vec![("(", ")"), ("[", "]"), ("{", "}")];
        let language =
            Language::new_from_vec(&pairs).expect("Failed to create language with multiple pairs");
        assert_eq!(
            language.get_k(),
            3,
            "get_k should return the correct number of parenthesis types"
        );
    }

    #[test]
    fn test_get_k_with_no_pairs() {
        let pairs = Vec::new();
        let language =
            Language::new_from_vec(&pairs).expect("Failed to create language with no pairs");
        assert_eq!(
            language.get_k(),
            0,
            "get_k should return 0 when no pairs are defined"
        );
    }

    #[test]
    fn test_is_open() {
        let pairs = vec![("{", "}")];
        let language = Language::new_from_vec(&pairs).unwrap();
        assert!(language.is_open("{"));
        assert!(!language.is_open("}"));
    }

    #[test]
    fn test_is_close() {
        let pairs = vec![("{", "}")];
        let language = Language::new_from_vec(&pairs).unwrap();
        assert!(language.is_close("}"));
        assert!(!language.is_close("{"));
    }

    #[test]
    fn test_is_valid() {
        let pairs = vec![("[", "]"), ("{", "}")];
        let language = Language::new_from_vec(&pairs).unwrap();
        let valid_word: Word = vec!["[", "{", "}", "]"];
        assert!(language.is_valid(&valid_word));
        let invalid_word: Word = vec!["[", "{", "]"];
        assert!(!language.is_valid(&invalid_word));
    }

    #[test]
    fn test_is_valid_incorrect_closing_order() {
        let pairs = vec![("(", ")"), ("[", "]")];
        let language = Language::new_from_vec(&pairs).unwrap();
        let invalid_word: Word = vec!["[", "(", "]", ")"];
        assert!(
            !language.is_valid(&invalid_word),
            "Word with incorrect closing order should be invalid"
        );
    }

    #[test]
    fn test_is_balanced() {
        let pairs = vec![("[", "]")];
        let language = Language::new_from_vec(&pairs).unwrap();
        let balanced_word: Word = vec!["[", "]"];
        assert!(language.is_balanced(&balanced_word));
        let unbalanced_word: Word = vec!["[", "["];
        assert!(!language.is_balanced(&unbalanced_word));
    }

    #[test]
    fn test_longest_valid_prefix() {
        let pairs = vec![("(", ")")];
        let language = Language::new_from_vec(&pairs).unwrap();
        let word: Word = vec!["(", "(", ")", ")", "(", ")"];
        assert_eq!(language.longest_valid_prefix(&word), 6);
    }

    #[test]
    fn test_longest_valid_prefix_incorrect_nesting() {
        let pairs = vec![("(", ")")];
        let language = Language::new_from_vec(&pairs).unwrap();
        let word: Word = vec!["(", ")", "(", ")", ")"];
        assert_eq!(
            language.longest_valid_prefix(&word),
            4,
            "Incorrect nesting should limit valid prefix length"
        );
    }

    #[test]
    fn test_longest_valid_prefix_entire_word() {
        let pairs = vec![("[", "]"), ("{", "}")];
        let language = Language::new_from_vec(&pairs).unwrap();
        let word: Word = vec!["[", "{", "}", "]"];
        assert_eq!(
            language.longest_valid_prefix(&word),
            word.len(),
            "The entire word should be a valid prefix"
        );
    }

    #[test]
    fn test_longest_valid_prefix_with_unmatched_open() {
        let pairs = vec![("(", ")")];
        let language = Language::new_from_vec(&pairs).unwrap();
        let word: Word = vec!["(", "("];
        assert_eq!(
            language.longest_valid_prefix(&word),
            0,
            "Word with unmatched open should have 0 as longest valid prefix"
        );
    }

    #[test]
    fn test_longest_valid_prefix_with_unmatched_close() {
        let pairs = vec![("(", ")")];
        let language = Language::new_from_vec(&pairs).unwrap();
        let word: Word = vec![")", "("];
        assert_eq!(
            language.longest_valid_prefix(&word),
            0,
            "Word with unmatched close should have 0 as longest valid prefix"
        );
    }

    #[test]
    fn test_shortest_completion() {
        let pairs = vec![("<", ">")];
        let language = Language::new_from_vec(&pairs).unwrap();
        let word: Word = vec!["<", "<"];
        let completion = language.shortest_completion(&word);
        assert_eq!(completion, vec![">", ">"]);
    }

    #[test]
    fn test_shortest_completion_already_complete() {
        let pairs = vec![("(", ")")];
        let language = Language::new_from_vec(&pairs).unwrap();
        let word: Word = vec!["(", ")", "(", ")"];
        let completion = language.shortest_completion(&word);
        assert!(
            completion.is_empty(),
            "Expected no completion for an already complete word"
        );
    }

    #[test]
    fn test_shortest_completion_complex_nesting() {
        let pairs = vec![("(", ")"), ("{", "}")];
        let language = Language::new_from_vec(&pairs).unwrap();
        let word: Word = vec!["(", "{", "(", ")"];
        let completion = language.shortest_completion(&word);
        assert_eq!(
            completion,
            vec!["}", ")"],
            "Completion did not match expected for complex nesting"
        );
    }

    #[test]
    fn test_shortest_completion_impossible_case() {
        let pairs = vec![("(", ")")];
        let language = Language::new_from_vec(&pairs).unwrap();
        let word: Word = vec![")", "("];
        let completion = language.shortest_completion(&word);
        assert!(
            completion.is_empty(),
            "Impossible case should return an empty completion"
        );
    }

    #[test]
    fn test_get_close() {
        let pairs = vec![("A", "B")];
        let language = Language::new_from_vec(&pairs).unwrap();
        assert_eq!(language.get_close("A"), Some("B"));
        assert_eq!(language.get_close("B"), None);
    }

    #[test]
    fn test_get_open() {
        let pairs = vec![("A", "B")];
        let language = Language::new_from_vec(&pairs).unwrap();
        assert_eq!(language.get_open("B"), Some("A"));
        assert_eq!(language.get_open("A"), None);
    }
}
