use thiserror::Error;

#[derive(Debug, Error, PartialEq)]
pub enum LexError {
    #[error("Unexpected character '{ch}' at position {pos}")]
    UnexpectedCharacter { pos: usize, ch: char },

    #[error("Invalid number at {start}..{end}")]
    InvalidNumber { start: usize, end: usize },
}
