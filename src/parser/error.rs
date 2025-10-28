use thiserror::Error;

use crate::lexer::{error::LexError, token::Token};

#[derive(Debug, Clone, Error)]
pub enum ParseError<'a> {
    #[error("Unexpected token: expected {expected}, found {found:?}")]
    UnexpectedToken {
        expected: String,
        found: Option<Token<'a>>,
    },

    #[error("Unexpected end of input")]
    UnexpectedEOF,

    #[error("Lexer error: {0}")]
    LexerError(#[from] LexError),
}
