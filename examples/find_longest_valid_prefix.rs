use dyck::{Language, Word};

fn main() {
    // Define pairs of tokens for the language
    let pairs = vec![("(", ")")];
    let language = Language::new_from_vec(&pairs).expect("Failed to create language");

    // Define a word to check
    let word: Word = vec!["(", "(", ")", ")", "(", ")"];
    let prefix_length = language.longest_valid_prefix(&word);

    // Print length of the longest valid prefix
    println!(
        "The length of the longest valid prefix is: {}",
        prefix_length
    );
}
