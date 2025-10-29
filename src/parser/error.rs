use std::fmt;

use thiserror::Error;

use miette::{Diagnostic, SourceSpan};

use crate::lexer::{
    error::LexError,
    token::{Span, TokenKind},
};

#[derive(Debug)]
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

impl SpanLabel {
    pub fn into_source_span(self) -> Option<SourceSpan> {
        self.0.map(|span| span.into_source_span())
    }
}

#[derive(Debug, Error)]
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

#[derive(Debug, Error, Diagnostic)]
pub enum ParseDiagnostic {
    #[error("Unexpected token: expected {expected}, found {found:?}")]
    #[diagnostic(code(parse::unexpected_token))]
    UnexpectedToken {
        expected: String,
        found: Option<String>,
        #[label("unexpected token")]
        span: Option<SourceSpan>,
    },

    #[error("Lexer error: {error}")]
    #[diagnostic(code(parse::lexer_error))]
    LexerError {
        #[source]
        error: LexError,
        #[label("invalid input")]
        span: Option<SourceSpan>,
    },
}

impl<'a> ParseError<'a> {
    pub fn into_diagnostic(self) -> ParseDiagnostic {
        match self {
            ParseError::UnexpectedToken {
                expected,
                found,
                span,
            } => ParseDiagnostic::UnexpectedToken {
                expected,
                found: found.map(|tok| tok.to_string()),
                span: span.into_source_span(),
            },
            ParseError::LexerError(error) => ParseDiagnostic::LexerError {
                span: error.span().map(|sp| sp.into_source_span()),
                error,
            },
        }
    }
}
