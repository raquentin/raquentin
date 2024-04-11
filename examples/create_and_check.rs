use dyck::{Language, Word};

fn main() {
    // define pairs of tokens for the language
    let pairs = vec![("(", ")"), ("[", "]"), ("{", "}")];
    let language = Language::new_from_vec(&pairs).expect("failed to create language");

    // define a word to check
    let word: Word = vec!["(", "[", "]", "(", ")", ")"];

    // check if the word is a valid Dyck word
    if language.is_valid(&word) {
        println!("the word is a valid Dyck word");
    } else {
        println!("the word is not a valid Dyck word");
    }
}
