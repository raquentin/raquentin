use dyck::{Language, Word};

#[cfg(test)]
mod core_tests {
    use super::*;

    #[test]
    fn new_from_vec_no_duplicates() {
        let pairs = vec![("(", ")"), ("[", "]")];
        let language = Language::new_from_vec(&pairs).expect("failed to create language");
        assert_eq!(language.get_k(), 2);
    }

    #[test]
    fn new_from_arr_no_duplicates() {
        let pairs = [("(", ")"), ("[", "]")];
        let language = Language::new_from_arr(&pairs).expect("failed to create language");
        assert_eq!(language.get_k(), 2);
    }

    #[test]
    fn new_from_vec_with_duplicate_opening_tokens() {
        let pairs = vec![("(", ")"), ("(", "]")];
        assert!(
            Language::new_from_vec(&pairs).is_err(),
            "constructor should fail with duplicate opening tokens"
        );
    }

    #[test]
    fn new_from_vec_with_duplicate_closing_tokens() {
        let pairs = vec![("(", ")"), ("{", ")")];
        assert!(
            Language::new_from_vec(&pairs).is_err(),
            "constructor should fail with duplicate closing tokens"
        );
    }

    #[test]
    fn new_from_vec_with_duplicate_pairs() {
        let pairs = vec![("(", ")"), ("(", ")")];
        assert!(
            Language::new_from_vec(&pairs).is_err(),
            "constructor should fail with duplicate pairs"
        );
    }

    #[test]
    fn get_k_with_single_pair() {
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
    fn get_k_with_multiple_pairs() {
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
    fn get_k_with_no_pairs() {
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
    fn is_open() {
        let pairs = vec![("{", "}")];
        let language = Language::new_from_vec(&pairs).unwrap();
        assert!(language.is_open("{"));
        assert!(!language.is_open("}"));
    }

    #[test]
    fn is_close() {
        let pairs = vec![("{", "}")];
        let language = Language::new_from_vec(&pairs).unwrap();
        assert!(language.is_close("}"));
        assert!(!language.is_close("{"));
    }

    #[test]
    fn is_valid() {
        let pairs = vec![("[", "]"), ("{", "}")];
        let language = Language::new_from_vec(&pairs).unwrap();
        let valid_word: Word = vec!["[", "{", "}", "]"];
        assert!(language.is_valid(&valid_word));
        let invalid_word: Word = vec!["[", "{", "]"];
        assert!(!language.is_valid(&invalid_word));
    }

    #[test]
    fn is_valid_empty_word() {
        let pairs = vec![("(", ")")];
        let language = Language::new_from_vec(&pairs).unwrap();
        assert!(
            language.is_valid(&Vec::new()),
            "An empty word should be considered valid"
        );
    }

    #[test]
    fn is_valid_incorrect_closing_order() {
        let pairs = vec![("(", ")"), ("[", "]")];
        let language = Language::new_from_vec(&pairs).unwrap();
        let invalid_word: Word = vec!["[", "(", "]", ")"];
        assert!(
            !language.is_valid(&invalid_word),
            "Word with incorrect closing order should be invalid"
        );
    }

    #[test]
    fn is_balanced() {
        let pairs = vec![("[", "]")];
        let language = Language::new_from_vec(&pairs).unwrap();
        let balanced_word: Word = vec!["[", "]"];
        assert!(language.is_balanced(&balanced_word));
        let unbalanced_word: Word = vec!["[", "["];
        assert!(!language.is_balanced(&unbalanced_word));
    }

    #[test]
    fn appending_to_valid_sequence_returns_same_sequence() {
        let pairs = vec![("(", ")"), ("[", "]")];
        let language = Language::new_from_vec(&pairs).unwrap();
        let word: Word = vec!["(", "[", "]", ")"]; // Already valid.

        let result = language.shortest_validating_appendage(&word).unwrap();

        assert_eq!(
            result, word,
            "Appending to an already valid sequence should return the sequence unchanged."
        );
    }

    #[test]
    fn appending_completes_sequence_needing_closures() {
        let pairs = vec![("(", ")"), ("[", "]")];
        let language = Language::new_from_vec(&pairs).unwrap();
        let word: Word = vec!["(", "[", "["]; // Needs "]])" to be complete.

        let result = language.shortest_validating_appendage(&word).unwrap();
        let expected_completion: Word = vec!["(", "[", "[", "]", "]", ")"];

        assert_eq!(
            result, expected_completion,
            "The completion should correctly close all open brackets."
        );
    }

    #[test]
    fn error_for_leading_close_token() {
        let pairs = vec![("(", ")"), ("[", "]")];
        let language = Language::new_from_vec(&pairs).unwrap();
        let word: Word = vec![")", "(", "[", "]"]; // Invalid start.

        let result = language.shortest_validating_appendage(&word);

        assert!(
            result.is_err(),
            "Should return an error for words starting with a closing token."
        );
    }

    #[test]
    fn appending_to_empty_sequence_returns_empty() {
        let pairs = vec![("(", ")"), ("[", "]")];
        let language = Language::new_from_vec(&pairs).unwrap();
        let word: Word = vec![]; // Empty.

        let result = language.shortest_validating_appendage(&word).unwrap();

        assert!(
            result.is_empty(),
            "Appending to an empty sequence should return an empty sequence."
        );
    }

    #[test]
    fn appending_corrects_sequence_with_multiple_unmatched_opens() {
        let pairs = vec![("(", ")"), ("[", "]")];
        let language = Language::new_from_vec(&pairs).unwrap();
        let word: Word = vec!["(", "[", "(", "["]; // Needs "])])" to be complete.

        let result = language.shortest_validating_appendage(&word).unwrap();
        let expected_completion: Word = vec!["(", "[", "(", "[", "]", ")", "]", ")"];

        assert_eq!(
            result, expected_completion,
            "The completion should correctly close all open brackets in reverse order."
        );
    }

    #[test]
    fn longest_valid_prefix() {
        let pairs = vec![("(", ")")];
        let language = Language::new_from_vec(&pairs).unwrap();
        let word: Word = vec!["(", "(", ")", ")", "(", ")"];
        assert_eq!(language.longest_valid_prefix(&word), 6);
    }

    #[test]
    fn longest_valid_prefix_empty_word() {
        let pairs = vec![("(", ")")];
        let language = Language::new_from_vec(&pairs).unwrap();
        assert_eq!(
            language.longest_valid_prefix(&Vec::new()),
            0,
            "The longest valid prefix of an empty word should be 0"
        );
    }

    #[test]
    fn longest_valid_prefix_incorrect_nesting() {
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
    fn longest_valid_prefix_entire_word() {
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
    fn longest_valid_prefix_with_unmatched_open() {
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
    fn longest_valid_prefix_with_unmatched_close() {
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
    fn get_close() {
        let pairs = vec![("A", "B")];
        let language = Language::new_from_vec(&pairs).unwrap();
        assert_eq!(language.get_close("A"), Some("B"));
        assert_eq!(language.get_close("B"), None);
    }

    #[test]
    fn get_open() {
        let pairs = vec![("A", "B")];
        let language = Language::new_from_vec(&pairs).unwrap();
        assert_eq!(language.get_open("B"), Some("A"));
        assert_eq!(language.get_open("A"), None);
    }

    #[test]
    fn inverse_operations_get_open_get_close() {
        let pairs = vec![("A", "B"), ("X", "Y")];
        let language = Language::new_from_vec(&pairs).unwrap();

        for (open, close) in pairs {
            assert_eq!(
                language.get_close(open),
                Some(close),
                "get_close did not return the correct closing token for {}",
                open
            );
            assert_eq!(
                language.get_open(close),
                Some(open),
                "get_open did not return the correct opening token for {}",
                close
            );
        }
    }
}
