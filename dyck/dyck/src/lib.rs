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
//! // define pairs of tokens for the language
//! let pairs = vec![("(", ")"), ("[", "]"), ("{", "}")];
//! let language = dyck::Language::new_from_vec(&pairs).unwrap();
//! let word: dyck::Word<&str> = vec!["(", "[", "]", "(", ")", ")"];
//! assert_eq!(language.is_valid(&word), true);
//! ```
//!
//! See /examples in the repository root for more examples.

use std::collections::HashMap;

#[cfg(feature = "derive")]
pub use dyck_derive::DyckToken;

/// A trait implementable by user-defined enums to gain compatiblity with all Dyck functions.
/// This trait alias is available as a derive macro via the derive feature.
pub trait DyckToken: Clone + Copy + Eq + std::hash::Hash {}

impl DyckToken for &str {}

/// A word is a sequence of tokens to be evaluated relative to a Dyck language.
/// It is not necessarily a dyck word, just a dyck candidate.
pub type Word<T> = Vec<T>;

/// Stores the context to be used to run algorithms on potential dyck words.
pub struct Language<T>
where
    T: DyckToken,
{
    alphabet: Vec<T>,
    open_to_close: HashMap<T, T>,
    close_to_open: HashMap<T, T>,
}

impl<T> Language<T>
where
    T: DyckToken,
{
    /// Creates a new Dyck context from a vector of pairs.
    pub fn new_from_vec(pairs: &Vec<(T, T)>) -> Result<Self, &'static str> {
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

    /// Creates a new Dyck context from a slice of pairs.
    pub fn new_from_arr(pairs: &[(T, T)]) -> Result<Self, &'static str> {
        Self::new_from_vec(&pairs.to_vec())
    }

    // Returns k, the number of parenthesis types in the alphabet.
    pub fn get_k(&self) -> usize {
        self.alphabet.len() / 2
    }

    /// Checks if a token is an opening token.
    pub fn is_open(&self, token: T) -> bool {
        self.open_to_close.contains_key(&token)
    }

    /// Checks if a token is a closing token.
    pub fn is_close(&self, token: T) -> bool {
        self.close_to_open.contains_key(&token)
    }

    /// Checks if a word is a valid Dyck word.
    pub fn is_valid(&self, word: &Word<T>) -> bool {
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
    pub fn is_balanced(&self, word: &Word<T>) -> bool {
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
    pub fn longest_valid_prefix(&self, word: &Word<T>) -> usize {
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
    /// Returns the validating shortest appendage, or None if no such appendage exists.
    pub fn shortest_validating_appendage(&self, word: &Word<T>) -> Option<Word<T>> {
        let mut stack = Vec::new();
        let mut completed_word = word.clone();

        for &token in word {
            if self.is_open(token) {
                stack.push(token);
            } else if self.is_close(token) {
                if let Some(&last) = stack.last() {
                    if self.open_to_close[&last] == token {
                        stack.pop();
                    }
                } else if stack.is_empty() {
                    return None;
                }
            }
        }

        for token in stack.into_iter().rev() {
            if let Some(close_token) = self.get_close(token) {
                completed_word.push(close_token);
            }
        }

        Some(completed_word)
    }

    /// Finds the closing token corresponding to an open token.
    pub fn get_close(&self, open: T) -> Option<T> {
        self.open_to_close.get(&open).copied()
    }

    /// Finds the opening token corresponding to a closing token.
    pub fn get_open(&self, close: T) -> Option<T> {
        self.close_to_open.get(&close).copied()
    }
}
