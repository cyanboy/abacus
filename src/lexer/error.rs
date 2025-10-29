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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unexpected_character_span_is_single_byte() {
        let err = LexError::UnexpectedCharacter { pos: 4, ch: '@' };
        let span = err.span().expect("span present");
        assert_eq!(span.start, 4);
        assert_eq!(span.end, 5);
    }

    #[test]
    fn invalid_number_span_preserved() {
        let err = LexError::InvalidNumber { start: 2, end: 7 };
        let span = err.span().expect("span present");
        assert_eq!(span.start, 2);
        assert_eq!(span.end, 7);
    }
}
