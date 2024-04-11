//! A library for running Dyck algorithms over languages of generic <Token> string slice types.
//!
//! This module provides functionalities to create and manipulate languages
//! based on user-defined tokens, allowing for the evaluation and transformation
//! of sequences of tokens (words) according to the rules of Dyck languages.
//!
//! ## Usage
//!
//! Add `dyck` to your `Cargo.toml`:
//!
//! ```toml
//! [dependencies]
//! dyck = "0.1"
//! ```
//! ## Example: Creating a Dyck language and checking if a word is valid.
//!
//! ```rust
//! use dyck::{Language, Word};
//!
//! fn main() {
//!     // Define pairs of tokens for the language
//!     let pairs = vec![("(", ")"), ("[", "]"), ("{", "}")];
//!     let language = Language::new_from_vec(&pairs).expect("Failed to create language");
//!
//!     // Define a word to check
//!     let word: Word = vec!["(", "[", "]", "(", ")", ")"];
//!
//!     // Check if the word is a valid Dyck word
//!     if language.is_valid(&word) {
//!         println!("The word is a valid Dyck word.");
//!     } else {
//!         println!("The word is not a valid Dyck word.");
//!     }
//! }
//! ```
//!
//! See /examples for more examples.

use std::collections::HashMap;

/// Allows dynamic Dyck alphabets for compatibility with user-defined tokens.
type Token = &'static str;

/// Stores the context to be used to run algorithms on potential dyck words.
pub struct Language<Token> {
    alphabet: Vec<Token>,
    open_to_close: HashMap<Token, Token>,
    close_to_open: HashMap<Token, Token>,
}

impl Language<Token> {
    /// Creates a new Dyck context from a vector of pairs.
    pub fn new_from_vec(pairs: &Vec<(Token, Token)>) -> Result<Self, &'static str> {
        let mut alphabet = Vec::new();
        let mut open_to_close = HashMap::new();
        let mut close_to_open = HashMap::new();
        for &(open, close) in pairs {
            if alphabet.contains(&open) || alphabet.contains(&close) {
                return Err("Duplicate token in the alphabet.");
            }
            alphabet.push(open);
            alphabet.push(close);
            open_to_close.insert(open, close);
            close_to_open.insert(close, open);
        }
        Ok(Self {
            alphabet,
            open_to_close,
            close_to_open,
        })
    }

    // Returns k, the number of parenthesis types in the alphabet.
    pub fn get_k(&self) -> usize {
        self.alphabet.len() / 2
    }

    /// Creates a new Dyck context from a slice of pairs.
    pub fn new_from_array(pairs: &[(Token, Token)]) -> Result<Self, &'static str> {
        Self::new_from_vec(&pairs.to_vec())
    }

    /// Checks if a token is an opening token.
    pub fn is_open(&self, token: Token) -> bool {
        self.open_to_close.contains_key(&token)
    }

    /// Checks if a token is a closing token.
    pub fn is_close(&self, token: Token) -> bool {
        self.close_to_open.contains_key(&token)
    }

    /// Checks if a word is a valid Dyck word.
    pub fn is_valid(&self, word: &Word) -> bool {
        let mut stack = Vec::new();
        for &token in word {
            if self.is_open(token) {
                stack.push(token);
            } else if self.is_close(token) {
                if let Some(&last) = stack.last() {
                    if self.open_to_close[&last] == token {
                        stack.pop();
                    } else {
                        return false;
                    }
                } else {
                    return false;
                }
            }
        }
        stack.is_empty()
    }

    /// Checks whether a word has balanced open and close tokens, but not necessarily in the
    /// correct order.
    pub fn is_balanced(&self, word: &Word) -> bool {
        let mut count = 0;
        for &token in word {
            if self.is_open(token) {
                count += 1;
            } else if self.is_close(token) {
                count -= 1;
            }
            if count < 0 {
                return false;
            }
        }
        count == 0
    }

    /// Finds the length of the longest valid prefix of a word.
    pub fn longest_valid_prefix(&self, word: &Word) -> usize {
        let mut stack = Vec::new();
        let mut length = 0;

        for (i, &token) in word.iter().enumerate() {
            if self.is_open(token) {
                stack.push(token);
            } else if self.is_close(token) {
                if let Some(&last) = stack.last() {
                    if self.open_to_close[&last] == token {
                        stack.pop();
                        if stack.is_empty() {
                            length = i + 1;
                        }
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            }
        }

        length
    }

    /// Finds the shortest completion of a word to make it a valid Dyck word.
    pub fn shortest_completion(&self, word: &Word) -> Vec<Token> {
        let mut stack = Vec::new();

        for &token in word {
            if self.is_open(token) {
                stack.push(token);
            } else if self.is_close(token) {
                if let Some(&last) = stack.last() {
                    if self.open_to_close[&last] == token {
                        stack.pop();
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            }
        }

        stack
            .into_iter()
            .rev()
            .filter_map(|token| self.get_close(token))
            .collect()
    }

    /// Finds the closing token corresponding to an open token.
    pub fn get_close(&self, open: Token) -> Option<Token> {
        self.open_to_close.get(&open).copied()
    }

    /// Finds the opening token corresponding to a closing token.
    pub fn get_open(&self, close: Token) -> Option<Token> {
        self.close_to_open.get(&close).copied()
    }
}

/// A word is a sequence of tokens to be evaluated relative to a Dyck language.
/// It is not necessarily a dyck word, just a dyck candidate.
pub type Word = Vec<Token>;
