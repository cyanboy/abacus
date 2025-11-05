use thiserror::Error;

use miette::{Diagnostic, SourceSpan};

use crate::lexer::{error::LexError, token::Span};

#[derive(Debug, Error, Diagnostic)]
pub enum ParseError {
    #[error("Unexpected token: expected {expected}, found {found}")]
    UnexpectedToken {
        expected: String,
        found: String,
        #[label("unexpected token")]
        span: Option<SourceSpan>,
    },

    #[error("Lexer error: {error}")]
    LexerError {
        #[source]
        error: LexError,
        #[label("invalid input")]
        span: Option<SourceSpan>,
    },
}

impl ParseError {
    pub fn unexpected(
        expected: impl Into<String>,
        found: Option<String>,
        span: Option<Span>,
    ) -> Self {
        ParseError::UnexpectedToken {
            expected: expected.into(),
            found: format_found(found),
            span: span.map(Span::into_source_span),
        }
    }

    pub fn unexpected_token(
        expected: impl Into<String>,
        token: Option<&crate::lexer::token::Token<'_>>,
    ) -> Self {
        let found = token.map(|tok| tok.kind.to_string());
        let span = token.map(|tok| tok.span);
        Self::unexpected(expected, found, span)
    }
}

fn format_found(found: Option<String>) -> String {
    match found {
        Some(f) => format!("'{f}'"),
        None => "end of input".to_string(),
    }
}

impl From<LexError> for ParseError {
    fn from(error: LexError) -> Self {
        let span = error.span().map(Span::into_source_span);
        ParseError::LexerError { error, span }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::token::{Span, Token, TokenKind};

    #[test]
    fn unexpected_token_uses_owned_data() {
        let token = Token::new(TokenKind::Identifier("foo"), Span::new(3, 6));
        let err = ParseError::unexpected_token("identifier", Some(&token));
        match err {
            ParseError::UnexpectedToken {
                expected,
                found,
                span: Some(span),
            } => {
                assert_eq!(expected, "identifier");
                assert_eq!(found, "'foo'");
                let offset: usize = span.offset().into();
                assert_eq!(offset, 3);
                assert_eq!(span.len(), 3);
            }
            other => panic!("unexpected error: {other:?}"),
        }
    }

    #[test]
    fn lexer_error_records_span() {
        let err = ParseError::from(LexError::InvalidNumber { start: 1, end: 5 });
        match err {
            ParseError::LexerError {
                span: Some(span), ..
            } => {
                let offset: usize = span.offset().into();
                assert_eq!(offset, 1);
                assert_eq!(span.len(), 4);
            }
            other => panic!("unexpected error: {other:?}"),
        }
    }
}
