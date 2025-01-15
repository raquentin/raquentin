# Dyck ()[]{}

This is a Rust crate for defining Dyck languages from ASCII characters (like the traditional ()[]{}) or from instanced of an enum deriving the exported `DyckToken` trait.

## Features
- Functions for verifying Dyck words, finding Dyck prefixes, finding smallest Dyck-verifying appendages, and more.
- A `#[derive(DyckToken)]` procedural macro for constructing languages and Dyck words from any struct or primitive.
- Zero dependencies in the `dyck` core (`dyck-derive` needs `syn` and `quote` for the `DyckToken` macro).

## Usage

You can use this crate with either `&str` string slices or with custom user-defined enum instance tokens.

The former allows for easy construction of typical Dyck alphabets consisting of "()", "[]", "{}", and even custom string pairs like "<>" or "<3":

```rust
let pairs = vec![("(", ")"), ("[", "]"), ("{", "}")];
let language = dyck::Language::new_from_vec(&pairs).unwrap("");
let word: dyck::Word<&str> = vec!["(", "[", "]", "(", ")", ")"];
assert_eq!(language.is_valid(&word), true);
```

The latter is more practical for use with existing programming languages or static analysis tools with lexers defining tokens as instances of a custom Token enum:

```rust
// requires features = ["derive"] in Cargo.toml
#[derive(dyck::DyckToken)]
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

let language = dyck::Language::new_from_vec(&pairs);
assert_eq!(language.get_k(), 2);
```

Either way, the all `dyck` functions and objects are built for a generic `T: DyckToken` and will run identically regardless of token type.

## License

This crate is licensed under the MIT License, see `LICENSE.md`.
