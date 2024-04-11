use dyck::{Language, Word};

fn main() {
    // define pairs of tokens for the language
    let pairs = vec![("(", ")")];
    let language = Language::new_from_vec(&pairs).expect("failed to create language");

    // define a word to check
    let word: Word<&str> = vec!["(", "(", ")", ")", "(", ")"];
    let prefix_length = language.longest_valid_prefix(&word);

    // print length of the longest valid prefix
    println!(
        "the length of the longest valid prefix is: {}",
        prefix_length
    );
}
