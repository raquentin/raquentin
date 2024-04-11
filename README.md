# dyck ()[]{}

This is a Rust crate for defining Dyck languages and running Dyck (and soon, InterDyck) algorithms on them. It enables the creation of languages with customizable token pairs (beyond ascii parenthesis), providing functionalities such as validity checks, shortest completion finding, and more, making it ideal for applications in compiler construction, parsing, and related fields.

## Usage

```rust
use dyck::{Language, Word};

fn main() {
    let pairs = vec![("(", ")"), ("[", "]"), ("{", "}")];
    let language = Language::new_from_vec(&pairs).expect("Failed to create language");
    let word: Word = vec!["(", "[", "]", "(", ")", ")"];

    if language.is_valid(&word) {
        println!("The word is a valid Dyck word.");
    } else {
        println!("The word is not a valid Dyck word.");
    }
}
```

More examples are available in the `/examples` dir.

## License

`dyck` is licensed under the MIT License, see `LICENSE.md`
