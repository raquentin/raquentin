# dyck ()[]{}

This is a Rust crate for defining Dyck languages and running Dyck (and soon, InterDyck) algorithms on them. It enables the creation of languages with customizable token pairs (beyond ascii parenthesis), providing functionalities such as validity checks, shortest completion finding, and more, making it ideal for applications in compiler construction, parsing, and related fields.

## Features
- Functions for verifying Dyck words, finding Dyck prefixes, finding smallest Dyck-verifying appendages, and more.
- A `#[derive(DyckToken)]` procedural macro for constructing languages and Dyck words from any struct or primitive.
- Coming soon: graph-based InterDyck algorithms.

## Usage

There are two ways to use the `dyck` crate: with `&str` string slices or with custom user-defined enum instance tokens.

The former allows for easy construction of typical Dyck alphabets consisting of "()", "[]", "{}", and even custom string pairs like "<>" or "<3":

```rust
use dyck::{Language, Word};

let pairs = vec![("(", ")"), ("[", "]"), ("{", "}")];
let language = Language::new_from_vec(&pairs).expect("failed to create language");
let word: Word = vec!["(", "[", "]", "(", ")", ")"];

if language.is_valid(&word) {
    println!("the word is a valid Dyck word");
} else {
    println!("the word is not a valid Dyck word");
}
```

The latter is more practical for use with existing programming languages or similar programs, particularly those with lexers defining tokens as instances of a custom Token enum:

```rust
// requires features = ["derive"] in Cargo.toml
use dyck::{Language, Word, DyckToken};

#[derive(DyckToken)]
enum Token {
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
}

let pairs = vec![
    (Token::OpenParen, Token::CloseParen),
    (Token::OpenBracket, Token::CloseBracket),
];
let language = Language::new_from_vec(&pairs).expect("failed to create language");
assert_eq!(language.get_k(), 2);
```

Either way, the all `dyck` functions and objects are built for a generic `T: DyckToken` and will run identically regardless of token type.

## License

This crate is licensed under the MIT License, see `LICENSE.md`.
