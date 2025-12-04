use std::{iter::Peekable, str::CharIndices};

pub mod error;
pub mod token;

use error::LexError;
use token::{Span, Token, TokenKind, TokenKind::*};

/// Streaming, zero-allocation lexer over `&str`.
/// Emits `Result<Token, LexError>` and implements `Iterator`.
#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    /// Original source slice (used for substring → number parse).
    s: &'a str,
    /// Cursor over `(byte_index, char)` with lookahead.
    chars: Peekable<CharIndices<'a>>,
}

impl<'a> Lexer<'a> {
    /// Construct a lexer from input.
    pub fn new(s: &'a str) -> Self {
        Self {
            s,
            chars: s.char_indices().peekable(),
        }
    }

    pub fn source_len(&self) -> usize {
        self.s.len()
    }

    /// Consume ASCII/Unicode whitespace.
    fn skip_whitespace(&mut self) {
        while self.chars.next_if(|&(_, c)| c.is_whitespace()).is_some() {}
    }

    /// Helper for two-char operators like `==`, `!=`, `<=`, `>=`, `||`, `&&`.
    /// If the next char equals `expected`, return operator `a` and consume it;
    /// otherwise return single-char operator `b` without consuming `expected`.
    #[inline]
    fn match_dual(
        &mut self,
        start: usize,
        first_len: usize,
        expected: char,
        two: TokenKind<'a>,
        one: TokenKind<'a>,
    ) -> (TokenKind<'a>, usize) {
        let mut end = start + first_len;
        if let Some(&(_, c)) = self.chars.peek()
            && c == expected
        {
            self.chars.next();
            end += c.len_utf8();
            return (two, end);
        }
        (one, end)
    }

    /// Core lexer step: skip ws, read one token, or return an error.
    /// Returns `None` at end of input.
    fn next_token(&mut self) -> Option<Result<Token<'a>, LexError>> {
        self.skip_whitespace();

        let (pos, ch) = self.chars.next()?; // EOF → None
        let first_len = ch.len_utf8();

        Some(match ch {
            // Single-char operators and separators
            '+' => Ok(Token::new(Plus, Span::new(pos, pos + first_len))),
            '-' => Ok(Token::new(Minus, Span::new(pos, pos + first_len))),
            '*' => Ok(Token::new(Star, Span::new(pos, pos + first_len))),
            '/' => Ok(Token::new(Slash, Span::new(pos, pos + first_len))),
            '%' => Ok(Token::new(Percent, Span::new(pos, pos + first_len))),
            '^' => Ok(Token::new(Caret, Span::new(pos, pos + first_len))),
            ',' => Ok(Token::new(Comma, Span::new(pos, pos + first_len))),
            '(' => Ok(Token::new(OpenParen, Span::new(pos, pos + first_len))),
            ')' => Ok(Token::new(CloseParen, Span::new(pos, pos + first_len))),

            // Two-char or fallback to single-char operators
            '=' => {
                let (tok, end) = self.match_dual(pos, first_len, '=', Eq, Assign);
                Ok(Token::new(tok, Span::new(pos, end)))
            }
            '!' => {
                let (tok, end) = self.match_dual(pos, first_len, '=', Ne, Bang);
                Ok(Token::new(tok, Span::new(pos, end)))
            }
            '<' => {
                let (tok, end) = self.match_dual(pos, first_len, '=', LtEq, Lt);
                Ok(Token::new(tok, Span::new(pos, end)))
            }
            '>' => {
                let (tok, end) = self.match_dual(pos, first_len, '=', GtEq, Gt);
                Ok(Token::new(tok, Span::new(pos, end)))
            }
            '|' => {
                let (tok, end) = self.match_dual(pos, first_len, '|', Or, BitOr);
                Ok(Token::new(tok, Span::new(pos, end)))
            }
            '&' => {
                let (tok, end) = self.match_dual(pos, first_len, '&', And, BitAnd);
                Ok(Token::new(tok, Span::new(pos, end)))
            }

            // Identifiers / keywords
            c if is_ident_start(c) => {
                let (tok, end) = self.identifier_or_keyword(pos, ch);
                Ok(Token::new(tok, Span::new(pos, end)))
            }

            // Numeric literals: int or float (with optional exponent)
            c if c.is_ascii_digit() => match self.numeric_literal(pos, ch) {
                Ok((tok, end)) => Ok(Token::new(tok, Span::new(pos, end))),
                Err(e) => Err(e),
            },

            // Unknown character
            _ => Err(LexError::UnexpectedCharacter { pos, ch }),
        })
    }

    /// Lex a numeric literal starting at `(start, first)`.
    /// Supports:
    /// - integers: `123`
    /// - floats: `1.`, `.1` is not accepted here, `1.2`, `1.2e-3`, `1e10`
    ///   Returns `Literal(Integer(_))` or `Literal(Float(_))`.
    fn numeric_literal(
        &mut self,
        start: usize,
        first: char,
    ) -> Result<(TokenKind<'a>, usize), LexError> {
        let mut end = start + first.len_utf8();
        let mut is_decimal = false;

        self.consume_digits(&mut end);

        if self.consume_fraction(&mut end) {
            is_decimal = true;
        }

        if self.consume_exponent(start, &mut end)? {
            is_decimal = true;
        }

        let slice = &self.s[start..end];
        let token = if is_decimal {
            slice
                .parse()
                .map(Float)
                .map_err(|_| LexError::InvalidNumber { start, end })
        } else {
            slice
                .parse()
                .map(Integer)
                .map_err(|_| LexError::InvalidNumber { start, end })
        }?;

        Ok((token, end))
    }

    /// Lex an identifier or boolean keyword.
    /// Accepts `[A-Za-z_][A-Za-z0-9_]*`. Maps `true/false` to boolean literals.
    fn identifier_or_keyword(&mut self, start: usize, first: char) -> (TokenKind<'a>, usize) {
        let mut end = start + first.len_utf8();

        while let Some(&(idx, c)) = self.chars.peek() {
            if is_ident_continue(c) {
                end = idx + c.len_utf8();
                self.chars.next();
            } else {
                break;
            }
        }

        let token = match &self.s[start..end] {
            "true" => Bool(true),
            "false" => Bool(false),
            ident => TokenKind::Identifier(ident),
        };
        (token, end)
    }

    fn consume_digits(&mut self, end: &mut usize) -> bool {
        let mut consumed = false;
        while let Some(&(idx, c)) = self.chars.peek() {
            if c.is_ascii_digit() {
                self.chars.next();
                *end = idx + c.len_utf8();
                consumed = true;
            } else {
                break;
            }
        }
        consumed
    }

    fn consume_fraction(&mut self, end: &mut usize) -> bool {
        if let Some(&(dot_idx, '.')) = self.chars.peek() {
            self.chars.next();
            *end = dot_idx + 1;
            self.consume_digits(end);
            return true;
        }
        false
    }

    fn consume_exponent(&mut self, start: usize, end: &mut usize) -> Result<bool, LexError> {
        if let Some(&(exp_idx, c @ ('e' | 'E'))) = self.chars.peek() {
            self.chars.next();
            *end = exp_idx + c.len_utf8();

            if let Some(&(sign_idx, sign @ ('+' | '-'))) = self.chars.peek() {
                self.chars.next();
                *end = sign_idx + sign.len_utf8();
            }

            if !self.consume_digits(end) {
                return Err(LexError::InvalidNumber { start, end: *end });
            }

            return Ok(true);
        }
        Ok(false)
    }
}

/// Start char for identifiers.
#[inline]
fn is_ident_start(ch: char) -> bool {
    ch.is_alphabetic() || ch == '_'
}

/// Continuation char for identifiers.
#[inline]
fn is_ident_continue(ch: char) -> bool {
    ch.is_alphanumeric() || ch == '_'
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, LexError>;

    /// Public iterator API forwards to `next_token`.
    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Assert that a single token is produced for `input`.
    fn assert_token(input: &str, expected: &TokenKind) {
        let mut lexer = Lexer::new(input);
        let spanned = lexer.next().unwrap().unwrap();
        assert_eq!(&spanned.kind, expected);
    }

    /// Collect all tokens and compare.
    fn assert_tokens(input: &str, expected: &[TokenKind]) {
        let lexer = Lexer::new(input);
        let result: Result<Vec<_>, _> = lexer.map(|t| t.map(|sp| sp.kind)).collect();
        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn test_single_tokens() {
        // Single-char operators and separators
        let cases = [
            ("+", Plus),
            ("-", Minus),
            ("*", Star),
            ("/", Slash),
            ("%", Percent),
            ("=", Assign),
            ("<", Lt),
            (">", Gt),
            ("!", Bang),
            ("|", BitOr),
            ("&", BitAnd),
            ("^", Caret),
            ("(", OpenParen),
            (")", CloseParen),
            (",", Comma),
        ];

        for (input, expected) in &cases {
            assert_token(input, expected);
        }
    }

    #[test]
    fn test_double_tokens() {
        // Paired operators recognized via `choose`
        let cases = [
            ("==", Eq),
            ("!=", Ne),
            ("<=", LtEq),
            (">=", GtEq),
            ("&&", And),
            ("||", Or),
        ];

        for (input, expected) in &cases {
            assert_token(input, expected);
        }
    }

    #[test]
    fn test_identifiers() {
        let cases = [
            ("x", Identifier("x")),
            ("xyz", Identifier("xyz")),
            ("_foo", Identifier("_foo")),
            ("bar_", Identifier("bar_")),
            ("BAZ", Identifier("BAZ")),
            ("x123", Identifier("x123")),
        ];

        for (input, expected) in &cases {
            assert_token(input, expected);
        }
    }

    #[test]
    fn test_integers() {
        let cases = [
            ("0", Integer(0)),
            ("1", Integer(1)),
            ("42", Integer(42)),
            ("123", Integer(123)),
        ];

        for (input, expected) in &cases {
            assert_token(input, expected);
        }
    }

    #[test]
    fn test_floats() {
        // Includes trailing-dot and scientific notation
        let cases = [
            ("0.1", Float(0.1)),
            ("13.0", Float(13.0)),
            ("1.", Float(1.)),
            ("1.2e-3", Float(1.2e-3)),
        ];

        for (input, expected) in &cases {
            assert_token(input, expected);
        }
    }

    #[test]
    fn test_booleans() {
        assert_token("true", &Bool(true));
        assert_token("false", &Bool(false));
    }

    #[test]
    fn test_keywords_as_prefix() {
        // Ensure keywords only match whole token
        assert_token("trueish", &Identifier("trueish"));
        assert_token("falsehood", &Identifier("falsehood"));
    }

    #[test]
    fn test_simple_expression() {
        assert_tokens(
            "x+1+1.2e10-42",
            &[
                Identifier("x"),
                Plus,
                Integer(1),
                Plus,
                Float(1.2e10),
                Minus,
                Integer(42),
            ],
        );
    }

    #[test]
    fn test_whitespace() {
        assert_tokens(" 2  +  2  ", &[Integer(2), Plus, Integer(2)]);
    }

    #[test]
    fn test_unexpected_character() {
        // '@' is not a recognized token start
        let mut lexer = Lexer::new("@");

        let first = lexer.next().unwrap();
        assert!(first.is_err());
        let err = first.unwrap_err();
        assert!(matches!(
            err,
            LexError::UnexpectedCharacter { pos: 0, ch: '@' }
        ));

        // After error, we are at EOF
        assert!(lexer.next().is_none());
    }

    #[test]
    fn invalid_number_span_covers_sign() {
        let mut lexer = Lexer::new("1e+");
        let err = lexer
            .next()
            .expect("token or error")
            .expect_err("expected invalid number");

        match err {
            LexError::InvalidNumber { start, end } => {
                assert_eq!(start, 0);
                assert_eq!(end, 3);
            }
            other @ LexError::UnexpectedCharacter { .. } => {
                panic!("unexpected error: {other:?}")
            }
        }
    }
}
