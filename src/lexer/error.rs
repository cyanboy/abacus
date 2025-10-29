use thiserror::Error;

use super::token::Span;

#[derive(Debug, Error, Clone)]
pub enum LexError {
    #[error("Unexpected character '{ch}' at position {pos}")]
    UnexpectedCharacter { pos: usize, ch: char },

    #[error("Invalid number at {start}..{end}")]
    InvalidNumber { start: usize, end: usize },
}

impl LexError {
    pub fn span(&self) -> Option<Span> {
        match *self {
            LexError::UnexpectedCharacter { pos, .. } => {
                Some(Span::new(pos, pos.saturating_add(1)))
            }
            LexError::InvalidNumber { start, end } => Some(Span::new(start, end)),
        }
    }
}
