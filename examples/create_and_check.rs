use dyck::{Language, Word};

fn main() {
    // Define pairs of tokens for the language
    let pairs = vec![("(", ")"), ("[", "]"), ("{", "}")];
    let language = Language::new_from_vec(&pairs).expect("Failed to create language");

    // Define a word to check
    let word: Word = vec!["(", "[", "]", "(", ")", ")"];

    // Check if the word is a valid Dyck word
    if language.is_valid(&word) {
        println!("The word is a valid Dyck word.");
    } else {
        println!("The word is not a valid Dyck word.");
    }
}
