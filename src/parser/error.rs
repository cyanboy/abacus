use thiserror::Error;

use crate::{LexError, Token};

#[derive(Debug, Error)]
pub enum ParseError<'a> {
    #[error("Unexpected token: expected {expected}, found {found:?}")]
    UnexpectedToken {
        expected: Token<'a>,
        found: Option<Token<'a>>,
    },

    #[error("Unexpected end of input")]
    UnexpectedEOF,

    #[error("Lexer error: {0}")]
    LexerError(#[from] LexError),
}
