use std::{iter::Peekable, str::CharIndices};

mod error;
mod token;

pub use error::LexError;
pub use token::TokenKind;

use crate::lexer::token::LiteralKind;

pub struct Lexer<'a> {
    s: &'a str,
    chars: Peekable<CharIndices<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(s: &'a str) -> Self {
        Self {
            s,
            chars: s.char_indices().peekable(),
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(&(_, ch)) = self.chars.peek() {
            if ch.is_whitespace() {
                self.chars.next();
            } else {
                break;
            }
        }
    }

    #[inline]
    fn choose(&mut self, expected: char, a: TokenKind<'a>, b: TokenKind<'a>) -> TokenKind<'a> {
        match self.chars.peek() {
            Some(&(_, c)) if c == expected => {
                self.chars.next();
                a
            }
            _ => b,
        }
    }
    fn next_token(&mut self) -> Option<Result<TokenKind<'a>, LexError>> {
        self.skip_whitespace();

        let (pos, ch) = self.chars.next()?;

        Some(match ch {
            '+' => Ok(TokenKind::Plus),
            '-' => Ok(TokenKind::Minus),
            '*' => Ok(TokenKind::Star),
            '/' => Ok(TokenKind::Slash),
            '%' => Ok(TokenKind::Percent),
            '^' => Ok(TokenKind::Caret),
            ',' => Ok(TokenKind::Comma),
            '(' => Ok(TokenKind::OpenParen),
            ')' => Ok(TokenKind::CloseParen),
            '[' => Ok(TokenKind::OpenBracket),
            ']' => Ok(TokenKind::CloseBracket),
            '=' => Ok(self.choose('=', TokenKind::EqEq, TokenKind::Eq)),
            '!' => Ok(self.choose('=', TokenKind::Ne, TokenKind::Bang)),
            '<' => Ok(self.choose('=', TokenKind::LtEq, TokenKind::Lt)),
            '>' => Ok(self.choose('=', TokenKind::GtEq, TokenKind::Gt)),
            '|' => Ok(self.choose('|', TokenKind::OrOr, TokenKind::Or)),
            '&' => Ok(self.choose('&', TokenKind::AndAnd, TokenKind::And)),
            c if is_ident_start(c) => Ok(self.identifier_or_keyword(pos, ch)),
            c if c.is_ascii_digit() => self.numeric_literal(pos, ch),
            _ => Err(LexError::UnexpectedCharacter { pos, ch }),
        })
    }

    fn numeric_literal(&mut self, start: usize, first: char) -> Result<TokenKind<'a>, LexError> {
        let mut end = start + first.len_utf8();
        let mut is_decimal = false;

        while let Some(&(idx, c)) = self.chars.peek() {
            match c {
                '0'..'9' => {
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

        if let Some(&(i, c @ ('e' | 'E'))) = self.chars.peek() {
            self.chars.next();
            end = i + c.len_utf8();
            is_decimal = true;

            if let Some(&(_, s @ ('+' | '-'))) = self.chars.peek() {
                self.chars.next();
                end = i + s.len_utf8();
            }

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

        let s = &self.s[start..end];
        if is_decimal {
            s.parse()
                .map(|n| TokenKind::Literal(LiteralKind::Float(n)))
                .map_err(|_| LexError::InvalidNumber { start, end })
        } else {
            s.parse()
                .map(|n| TokenKind::Literal(LiteralKind::Integer(n)))
                .map_err(|_| LexError::InvalidNumber { start, end })
        }
    }

    fn identifier_or_keyword(&mut self, start: usize, first: char) -> TokenKind<'a> {
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
            "true" => TokenKind::Literal(LiteralKind::Bool(true)),
            "false" => TokenKind::Literal(LiteralKind::Bool(false)),
            ident => TokenKind::Identifier(ident),
        }
    }
}

#[inline]
fn is_ident_start(ch: char) -> bool {
    ch.is_alphabetic() || ch == '_'
}

#[inline]
fn is_ident_continue(ch: char) -> bool {
    ch.is_alphanumeric() || ch == '_'
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<TokenKind<'a>, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use LiteralKind::*;
    use TokenKind::*;

    fn assert_token(input: &str, expected: TokenKind) {
        let mut lexer = Lexer::new(input);
        assert_eq!(lexer.next().unwrap().unwrap(), expected);
    }

    fn assert_tokens(input: &str, expected: Vec<TokenKind>) {
        let lexer = Lexer::new(input);
        let result: Result<Vec<_>, _> = lexer.collect();
        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn test_single_tokens() {
        let cases = [
            ("+", Plus),
            ("-", Minus),
            ("*", Star),
            ("/", Slash),
            ("%", Percent),
            ("=", Eq),
            ("<", Lt),
            (">", Gt),
            ("!", Bang),
            ("|", Or),
            ("&", And),
            ("^", Caret),
            ("(", OpenParen),
            (")", CloseParen),
            ("[", OpenBracket),
            ("]", CloseBracket),
            (",", Comma),
        ];

        for (input, expected) in cases {
            assert_token(input, expected);
        }
    }

    #[test]
    fn test_double_tokens() {
        let cases = [
            ("==", EqEq),
            ("!=", Ne),
            ("<=", LtEq),
            (">=", GtEq),
            ("&&", AndAnd),
            ("||", OrOr),
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
            ("2147483647", Literal(Integer(2147483647))),
        ];

        for (input, expected) in cases {
            assert_token(input, expected);
        }
    }

    #[test]
    fn test_floats() {
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
        assert_token("trueish", Identifier("trueish"));
        assert_token("falsehood", Identifier("falsehood"));
    }

    #[test]
    fn test_simple_expression() {
        assert_tokens(
            "x+1+1.2e10-42",
            vec![
                Identifier("x"),
                Plus,
                Literal(Integer(1)),
                Plus,
                Literal(Float(1.2e10)),
                Minus,
                Literal(Integer(42)),
            ],
        );
    }

    #[test]
    fn test_whitespace() {
        assert_tokens(
            " 2  +  2  ",
            vec![Literal(Integer(2)), Plus, Literal(Integer(2))],
        );
    }

    #[test]
    fn test_unexpected_character() {
        let mut lexer = Lexer::new("@");

        let first = lexer.next().unwrap();
        assert!(first.is_err());
        let err = first.unwrap_err();
        assert!(matches!(
            err,
            LexError::UnexpectedCharacter { pos: 0, ch: '@' }
        ));

        assert!(lexer.next().is_none());
    }
}
