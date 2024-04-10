use dyck::{Alphabet, Word};

fn main() {
    enum Token {
        OpenParen,
        CloseParen,
        OpenBracket,
        CloseBracket,
        OpenBrace,
        CloseBrace,
    }

    let alphabet = Alphabet::new_from_vec(&vec![(1, 2), (3, 4)]);
    let context = DyckContext::new_from_vec(&vec![(1, 2), (3, 4)]);
    let word = vec![1, 2, 3, 4];
    println!(
        "Is the word {:?} a Dyck word? {}",
        word,
        is_dyck(&word, &context)
    );
}
