use thiserror::Error;

use crate::{LexError, TokenKind};

#[derive(Debug, Error)]
pub enum ParseError<'a> {
    #[error("Unexpected token: expected {expected}, found {found:?}")]
    UnexpectedToken {
        expected: TokenKind<'a>,
        found: Option<TokenKind<'a>>,
    },

    #[error("Unexpected end of input")]
    UnexpectedEOF,

    #[error("Lexer error: {0}")]
    LexerError(#[from] LexError),
}
