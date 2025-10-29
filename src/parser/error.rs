use std::fmt;

use thiserror::Error;

use crate::lexer::{
    error::LexError,
    token::{Span, TokenKind},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SpanLabel(pub Option<Span>);

impl fmt::Display for SpanLabel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(span) = self.0 {
            write!(f, " at byte {}", span.start)
        } else {
            Ok(())
        }
    }
}

impl From<Option<Span>> for SpanLabel {
    fn from(value: Option<Span>) -> Self {
        SpanLabel(value)
    }
}

#[derive(Debug, Clone, Error)]
pub enum ParseError<'a> {
    #[error("Unexpected token: expected {expected}, found {found:?}{span}")]
    UnexpectedToken {
        expected: String,
        found: Option<TokenKind<'a>>,
        span: SpanLabel,
    },

    #[error("Lexer error: {0}")]
    LexerError(#[from] LexError),
}
