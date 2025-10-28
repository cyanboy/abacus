use std::{iter::Peekable, str::CharIndices};

pub mod error;
pub mod token;

use error::LexError;
use token::{LiteralKind::*, OperatorKind::*, SeparatorKind::*, Token, Token::*};

use crate::lexer::token::OperatorKind;

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

    /// Consume ASCII/Unicode whitespace.
    fn skip_whitespace(&mut self) {
        while self.chars.next_if(|&(_, c)| c.is_whitespace()).is_some() {}
    }

    /// Helper for two-char operators like `==`, `!=`, `<=`, `>=`, `||`, `&&`.
    /// If the next char equals `expected`, return operator `a` and consume it;
    /// otherwise return single-char operator `b` without consuming `expected`.
    #[inline]
    fn choose(&mut self, expected: char, a: OperatorKind, b: OperatorKind) -> Token<'a> {
        match self.chars.peek() {
            Some(&(_, c)) if c == expected => {
                self.chars.next();
                Operator(a)
            }
            _ => Operator(b),
        }
    }

    /// Core lexer step: skip ws, read one token, or return an error.
    /// Returns `None` at end of input.
    fn next_token(&mut self) -> Option<Result<Token<'a>, LexError>> {
        self.skip_whitespace();

        let (pos, ch) = self.chars.next()?; // EOF → None

        Some(match ch {
            // Single-char operators and separators
            '+' => Ok(Operator(Plus)),
            '-' => Ok(Operator(Minus)),
            '*' => Ok(Operator(Star)),
            '/' => Ok(Operator(Slash)),
            '%' => Ok(Operator(Percent)),
            '^' => Ok(Operator(Caret)),
            ',' => Ok(Separator(Comma)),
            '(' => Ok(Separator(OpenParen)),
            ')' => Ok(Separator(CloseParen)),
            '[' => Ok(Separator(OpenBracket)),
            ']' => Ok(Separator(CloseBracket)),

            // Two-char or fallback to single-char operators
            '=' => Ok(self.choose('=', Eq, Assign)),
            '!' => Ok(self.choose('=', Ne, Bang)),
            '<' => Ok(self.choose('=', LtEq, Lt)),
            '>' => Ok(self.choose('=', GtEq, Gt)),
            '|' => Ok(self.choose('|', Or, BitOr)),
            '&' => Ok(self.choose('&', And, BitAnd)),

            // Identifiers / keywords
            c if is_ident_start(c) => Ok(self.identifier_or_keyword(pos, ch)),

            // Numeric literals: int or float (with optional exponent)
            c if c.is_ascii_digit() => self.numeric_literal(pos, ch),

            // Unknown character
            _ => Err(LexError::UnexpectedCharacter { pos, ch }),
        })
    }

    /// Lex a numeric literal starting at `(start, first)`.
    /// Supports:
    /// - integers: `123`
    /// - floats: `1.`, `.1` is not accepted here, `1.2`, `1.2e-3`, `1e10`
    /// Returns `Literal(Integer(_))` or `Literal(Float(_))`.
    fn numeric_literal(&mut self, start: usize, first: char) -> Result<Token<'a>, LexError> {
        let mut end = start + first.len_utf8();
        let mut is_decimal = false;

        // Integer or fractional part
        while let Some(&(idx, c)) = self.chars.peek() {
            match c {
                '0'..='9' => {
                    end = idx + c.len_utf8();
                    self.chars.next();
                }
                '.' if !is_decimal => {
                    is_decimal = true;
                    end = idx + c.len_utf8();
                    self.chars.next();
                }
                _ => break,
            }
        }

        // Optional exponent part: [eE][+-]?[0-9]+
        if let Some(&(i, c @ ('e' | 'E'))) = self.chars.peek() {
            self.chars.next();
            end = i + c.len_utf8();
            is_decimal = true; // presence of exponent forces float

            // Optional sign in exponent
            if let Some(&(_, s @ ('+' | '-'))) = self.chars.peek() {
                self.chars.next();
                end = i + s.len_utf8();
            }

            // At least one digit of exponent
            let mut saw_digit = false;
            while let Some(&(i, d)) = self.chars.peek() {
                if d.is_ascii_digit() {
                    self.chars.next();
                    end = i + d.len_utf8();
                    saw_digit = true;
                } else {
                    break;
                }
            }

            if !saw_digit {
                return Err(LexError::InvalidNumber { start, end });
            }
        }

        // Parse the captured substring
        let s = &self.s[start..end];
        if is_decimal {
            s.parse()
                .map(|n| Literal(Float(n)))
                .map_err(|_| LexError::InvalidNumber { start, end })
        } else {
            s.parse()
                .map(|n| Literal(Integer(n)))
                .map_err(|_| LexError::InvalidNumber { start, end })
        }
    }

    /// Lex an identifier or boolean keyword.
    /// Accepts `[A-Za-z_][A-Za-z0-9_]*`. Maps `true/false` to boolean literals.
    fn identifier_or_keyword(&mut self, start: usize, first: char) -> Token<'a> {
        let mut end = start + first.len_utf8();

        while let Some(&(idx, c)) = self.chars.peek() {
            if is_ident_continue(c) {
                end = idx + c.len_utf8();
                self.chars.next();
            } else {
                break;
            }
        }

        match &self.s[start..end] {
            "true" => Literal(Bool(true)),
            "false" => Literal(Bool(false)),
            ident => Token::Identifier(ident),
        }
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
    fn assert_token(input: &str, expected: Token) {
        let mut lexer = Lexer::new(input);
        assert_eq!(lexer.next().unwrap().unwrap(), expected);
    }

    /// Collect all tokens and compare.
    fn assert_tokens(input: &str, expected: Vec<Token>) {
        let lexer = Lexer::new(input);
        let result: Result<Vec<_>, _> = lexer.collect();
        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn test_single_tokens() {
        // Single-char operators and separators
        let cases = [
            ("+", Operator(Plus)),
            ("-", Operator(Minus)),
            ("*", Operator(Star)),
            ("/", Operator(Slash)),
            ("%", Operator(Percent)),
            ("=", Operator(Assign)),
            ("<", Operator(Lt)),
            (">", Operator(Gt)),
            ("!", Operator(Bang)),
            ("|", Operator(BitOr)),
            ("&", Operator(BitAnd)),
            ("^", Operator(Caret)),
            ("(", Separator(OpenParen)),
            (")", Separator(CloseParen)),
            ("[", Separator(OpenBracket)),
            ("]", Separator(CloseBracket)),
            (",", Separator(Comma)),
        ];

        for (input, expected) in cases {
            assert_token(input, expected);
        }
    }

    #[test]
    fn test_double_tokens() {
        // Paired operators recognized via `choose`
        let cases = [
            ("==", Operator(Eq)),
            ("!=", Operator(Ne)),
            ("<=", Operator(LtEq)),
            (">=", Operator(GtEq)),
            ("&&", Operator(And)),
            ("||", Operator(Or)),
        ];

        for (input, expected) in cases {
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

        for (input, expected) in cases {
            assert_token(input, expected);
        }
    }

    #[test]
    fn test_integers() {
        let cases = [
            ("0", Literal(Integer(0))),
            ("1", Literal(Integer(1))),
            ("42", Literal(Integer(42))),
            ("123", Literal(Integer(123))),
        ];

        for (input, expected) in cases {
            assert_token(input, expected);
        }
    }

    #[test]
    fn test_floats() {
        // Includes trailing-dot and scientific notation
        let cases = [
            ("0.1", Literal(Float(0.1))),
            ("13.0", Literal(Float(13.0))),
            ("1.", Literal(Float(1.))),
            ("1.2e-3", Literal(Float(1.2e-3))),
        ];

        for (input, expected) in cases {
            assert_token(input, expected);
        }
    }

    #[test]
    fn test_booleans() {
        assert_token("true", Literal(Bool(true)));
        assert_token("false", Literal(Bool(false)));
    }

    #[test]
    fn test_keywords_as_prefix() {
        // Ensure keywords only match whole token
        assert_token("trueish", Identifier("trueish"));
        assert_token("falsehood", Identifier("falsehood"));
    }

    #[test]
    fn test_simple_expression() {
        assert_tokens(
            "x+1+1.2e10-42",
            vec![
                Identifier("x"),
                Operator(Plus),
                Literal(Integer(1)),
                Operator(Plus),
                Literal(Float(1.2e10)),
                Operator(Minus),
                Literal(Integer(42)),
            ],
        );
    }

    #[test]
    fn test_whitespace() {
        assert_tokens(
            " 2  +  2  ",
            vec![Literal(Integer(2)), Operator(Plus), Literal(Integer(2))],
        );
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
}
