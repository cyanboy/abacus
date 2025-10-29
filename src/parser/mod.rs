use std::iter::Peekable;

use crate::lexer::{
    Lexer,
    token::{Span, Token, TokenKind},
};

pub mod ast;
mod error;

use ast::*;
use error::{ParseError, SpanLabel};

/// Single-statement parser for a REPL input.
/// Wraps a `Lexer` that yields `Result<Token, LexError>` and exposes Pratt parsing.
pub struct Parser<'a> {
    /// Lookahead-capable stream of tokens (or lexer errors).
    lexer: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    /// Build a parser from a source string.
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            lexer: lexer.peekable(),
        }
    }

    /// Entry point. Parse exactly one statement.
    pub fn parse(&mut self) -> Result<Stmt, ParseError<'a>> {
        let stmt = self.parse_stmt()?;
        if let Some(tok) = self.peek()? {
            return Err(ParseError::UnexpectedToken {
                expected: "end of input".into(),
                found: Some(tok.kind.clone()),
                span: Some(tok.span).into(),
            });
        }
        Ok(stmt)
    }

    /// Peek next token without consuming. Propagate lexer errors.
    fn peek(&mut self) -> Result<Option<&Token<'a>>, ParseError<'a>> {
        match self.lexer.peek() {
            Some(Ok(tok)) => Ok(Some(tok)),
            Some(Err(e)) => Err(ParseError::LexerError(e.clone())),
            None => Ok(None),
        }
    }

    /// Consume one token. Propagate lexer errors.
    fn bump(&mut self) -> Result<Option<Token<'a>>, ParseError<'a>> {
        match self.lexer.next() {
            Some(Ok(tok)) => Ok(Some(tok)),
            Some(Err(e)) => Err(ParseError::LexerError(e)),
            None => Ok(None),
        }
    }

    /// If next token equals `expected`, consume it and return true.
    fn eat(&mut self, expected: TokenKind<'a>) -> Result<bool, ParseError<'a>> {
        if matches!(self.peek()?, Some(t) if t.kind == expected) {
            self.bump()?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn expect_token(&mut self, expected: TokenKind<'a>) -> Result<Token<'a>, ParseError<'a>> {
        let expected_str = expected.to_string();
        match self.bump()? {
            Some(tok) if tok.kind == expected => Ok(tok),
            Some(tok) => Err(ParseError::UnexpectedToken {
                expected: expected_str,
                found: Some(tok.kind),
                span: Some(tok.span).into(),
            }),
            None => Err(ParseError::UnexpectedToken {
                expected: expected_str,
                found: None,
                span: None.into(),
            }),
        }
    }

    /// Require the next token to be `expected`. Error otherwise.
    fn expect(&mut self, expected: TokenKind<'a>) -> Result<(), ParseError<'a>> {
        self.expect_token(expected)?;
        Ok(())
    }

    /// stmt := func_def | assignment | expr
    fn parse_stmt(&mut self) -> Result<Stmt, ParseError<'a>> {
        match self.peek()? {
            Some(tok) if matches!(tok.kind, TokenKind::Identifier(_)) => {
                self.parse_stmt_starting_with_ident()
            }
            _ => Ok(Stmt::Expression(self.parse_expr_bp(0)?)),
        }
    }

    /// Disambiguate after seeing a leading identifier:
    /// - `f(<patterns>) = <expr>` → function definition
    /// - `<name> = <expr>` → assignment
    /// - otherwise treat the identifier as the start of an expression
    fn parse_stmt_starting_with_ident(&mut self) -> Result<Stmt, ParseError<'a>> {
        // consume the leading name
        let (name, name_span) = match self.bump()? {
            Some(sp) => match sp.kind {
                TokenKind::Identifier(s) => (s.to_string(), sp.span),
                other => {
                    return Err(ParseError::UnexpectedToken {
                        expected: "identifier".into(),
                        found: Some(other),
                        span: Some(sp.span).into(),
                    });
                }
            },
            None => {
                return Err(ParseError::UnexpectedToken {
                    expected: "identifier".into(),
                    found: None,
                    span: None.into(),
                });
            }
        };

        // Function definition only if we see "( ... )" followed by '='.
        if matches!(self.peek()?, Some(tok) if tok.kind == TokenKind::OpenParen)
            && self.lookahead_func_def_after_params()?
        {
            self.expect(TokenKind::OpenParen)?;
            let params = self.parse_pattern_list()?;
            self.expect(TokenKind::Assign)?;
            let body = self.parse_expr_bp(0)?;
            return Ok(Stmt::FunctionDefinition {
                name,
                arms: vec![FuncArm { params, body }],
            });
        }

        // Assignment: name '=' expr
        if self.eat(TokenKind::Assign)? {
            let value = self.parse_expr_bp(0)?;
            return Ok(Stmt::Assignment { name, value });
        }

        // Otherwise it's an expression that began with an identifier.
        let lhs = Expr::Identifier(name, name_span);
        Ok(Stmt::Expression(self.parse_expr_bp_with_lhs(lhs, 0)?))
    }

    /// Parse `( p1, p2, ... )` after seeing the opening '(' already consumed.
    fn parse_pattern_list(&mut self) -> Result<Vec<Pattern>, ParseError<'a>> {
        let mut params = Vec::new();
        // Empty parameter list `()`.
        if self.eat(TokenKind::CloseParen)? {
            return Ok(params);
        }
        // One or more patterns separated by commas.
        loop {
            params.push(self.parse_pattern()?);
            if self.eat(TokenKind::Comma)? {
                continue;
            }
            self.expect(TokenKind::CloseParen)?;
            break;
        }
        Ok(params)
    }

    /// pattern := identifier | literal
    fn parse_pattern(&mut self) -> Result<Pattern, ParseError<'a>> {
        match self.bump()? {
            Some(Token { kind, span }) => match kind {
                TokenKind::Identifier(s) => Ok(Pattern::Identifier(s.to_string())),
                TokenKind::Integer(n) => Ok(Pattern::Lit(Literal::Int(n))),
                TokenKind::Float(x) => Ok(Pattern::Lit(Literal::Float(x))),
                TokenKind::Bool(b) => Ok(Pattern::Lit(Literal::Bool(b))),
                other => Err(ParseError::UnexpectedToken {
                    expected: "literal".to_string(),
                    found: Some(other),
                    span: Some(span).into(),
                }),
            },
            None => Err(ParseError::UnexpectedToken {
                expected: "literal".to_string(),
                found: None,
                span: None.into(),
            }),
        }
    }

    /// Pratt parse with given minimum binding power `min_bp`.
    /// Handles:
    /// - prefix unary ops: `!` and `-`
    /// - postfix call: `expr(args...)`
    /// - binary ops with precedence/associativity from `infix_bp`
    fn parse_expr_bp(&mut self, min_bp: u8) -> Result<Expr, ParseError<'a>> {
        let lhs = if let Some(tok) = self.peek()? {
            if let Some(op) = prefix_op(&tok.kind) {
                let op_tok = match self.bump()? {
                    Some(tok) => tok,
                    None => unreachable!("prefix operator vanished after peek"),
                };
                let rhs = self.parse_expr_bp(PREFIX_BP)?;
                let span = span_cover(op_tok.span, rhs.span());
                Expr::Unary {
                    op,
                    span,
                    rhs: Box::new(rhs),
                }
            } else {
                self.parse_primary()?
            }
        } else {
            return Err(ParseError::UnexpectedToken {
                expected: "expression".to_string(),
                found: None,
                span: None.into(),
            });
        };

        self.parse_expr_bp_with_lhs(lhs, min_bp)
    }

    /// Continue Pratt parsing when the caller already parsed an initial `lhs`.
    fn parse_expr_bp_with_lhs(
        &mut self,
        mut lhs: Expr,
        min_bp: u8,
    ) -> Result<Expr, ParseError<'a>> {
        loop {
            // Postfix call
            if matches!(self.peek()?, Some(tok) if tok.kind == TokenKind::OpenParen) {
                let open_tok = match self.bump()? {
                    Some(tok) => tok,
                    None => unreachable!("open paren vanished after peek"),
                };
                let mut args = Vec::new();
                let close_tok = if matches!(self.peek()?, Some(tok) if tok.kind == TokenKind::CloseParen)
                {
                    match self.bump()? {
                        Some(tok) => tok,
                        None => unreachable!("close paren expected"),
                    }
                } else {
                    loop {
                        args.push(self.parse_expr_bp(0)?);
                        if matches!(self.peek()?, Some(tok) if tok.kind == TokenKind::Comma) {
                            self.bump()?;
                            continue;
                        }
                        break;
                    }
                    self.expect_token(TokenKind::CloseParen)?
                };
                let callee_expr = lhs;
                let call_span = span_cover(callee_expr.span(), open_tok.span);
                let span = span_cover(call_span, close_tok.span);
                lhs = Expr::Call {
                    callee: Box::new(callee_expr),
                    args,
                    span,
                };
                continue;
            }

            let (op, lbp, rbp, op_span) = match self.peek()? {
                Some(tok) => match infix_bp(&tok.kind) {
                    Some((op, lbp, rbp)) => (op, lbp, rbp, tok.span),
                    None => break,
                },
                None => break,
            };

            if lbp < min_bp {
                break;
            }

            self.bump()?;
            let rhs = self.parse_expr_bp(rbp)?;
            let span = span_cover(lhs.span(), span_cover(op_span, rhs.span()));
            let left = lhs;
            lhs = Expr::Binary {
                lhs: Box::new(left),
                op,
                span,
                rhs: Box::new(rhs),
            };
        }

        Ok(lhs)
    }

    /// primary := literal | identifier | '(' expr ')'
    fn parse_primary(&mut self) -> Result<Expr, ParseError<'a>> {
        match self.bump()? {
            Some(Token {
                kind: TokenKind::Integer(n),
                span,
            }) => Ok(Expr::Lit(Literal::Int(n), span)),
            Some(Token {
                kind: TokenKind::Float(x),
                span,
            }) => Ok(Expr::Lit(Literal::Float(x), span)),
            Some(Token {
                kind: TokenKind::Bool(b),
                span,
            }) => Ok(Expr::Lit(Literal::Bool(b), span)),
            Some(Token {
                kind: TokenKind::Identifier(s),
                span,
            }) => Ok(Expr::Identifier(s.to_string(), span)),
            Some(Token {
                kind: TokenKind::OpenParen,
                span: open_span,
            }) => {
                let expr = self.parse_expr_bp(0)?;
                let close_tok = self.expect_token(TokenKind::CloseParen)?;
                let span = span_cover(open_span, close_tok.span);
                Ok(Expr::Group(Box::new(expr), span))
            }
            found => {
                let (tok, span): (Option<_>, SpanLabel) = match found {
                    Some(t) => (Some(t.kind), Some(t.span).into()),
                    None => (None, None.into()),
                };
                Err(ParseError::UnexpectedToken {
                    expected: "expression".to_string(),
                    found: tok,
                    span,
                })
            }
        }
    }

    /// Lookahead from after an identifier:
    /// Return true if the next tokens are a parameter list `(...)`
    /// and the following token is `=`, indicating a function definition.
    fn lookahead_func_def_after_params(&mut self) -> Result<bool, ParseError<'a>> {
        let mut snap = self.lexer.clone();
        // require '('
        match snap.next() {
            Some(Ok(Token {
                kind: TokenKind::OpenParen,
                ..
            })) => {}
            _ => return Ok(false),
        }
        // scan to matching ')'
        let mut depth = 1usize;
        while let Some(next) = snap.next() {
            let t = next.map_err(ParseError::LexerError)?;
            match t.kind {
                TokenKind::OpenParen => depth += 1,
                TokenKind::CloseParen => {
                    depth -= 1;
                    if depth == 0 {
                        break;
                    }
                }
                _ => {}
            }
        }
        if depth != 0 {
            // unmatched '(' → not confidently a func def; let real parse report it
            return Ok(false);
        }

        // expect '=' immediately after the ')'
        match snap.next() {
            Some(Ok(Token {
                kind: TokenKind::Assign,
                ..
            })) => Ok(true),
            _ => Ok(false),
        }
    }
}

fn span_cover(a: Span, b: Span) -> Span {
    Span::new(a.start.min(b.start), a.end.max(b.end))
}

/// Binding power for prefix operators. Must bind tighter than `* / %`.
const PREFIX_BP: u8 = 100;

/// Recognize prefix unary operators.
fn prefix_op(tok: &TokenKind) -> Option<UnaryOp> {
    match tok {
        TokenKind::Minus => Some(UnaryOp::Neg),
        TokenKind::Bang => Some(UnaryOp::Not),
        _ => None,
    }
}

/// Map a token to `(op, left_bp, right_bp)`.
/// Left-associative operators use `rbp = lbp + 1`.
fn infix_bp(tok: &TokenKind) -> Option<(BinOp, u8, u8)> {
    use BinOp::*;

    match tok {
        TokenKind::Or => Some((Or, 1, 2)),
        TokenKind::And => Some((And, 2, 3)),
        TokenKind::BitOr => Some((BitOr, 3, 4)),
        TokenKind::BitAnd => Some((BitAnd, 4, 5)),
        TokenKind::Caret => Some((Xor, 5, 6)),
        TokenKind::Eq | TokenKind::Ne => Some((Eq, 6, 7)), // adjust as needed per op
        TokenKind::Lt | TokenKind::LtEq | TokenKind::Gt | TokenKind::GtEq => Some((Lt, 7, 8)),
        TokenKind::Plus | TokenKind::Minus => Some((Add, 8, 9)),
        TokenKind::Star | TokenKind::Slash | TokenKind::Percent => Some((Mul, 9, 10)),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::token::Span;

    fn parse<'a>(input: &'a str) -> Result<Stmt, ParseError<'a>> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        parser.parse()
    }

    #[test]
    fn parses_binary_precedence() {
        let stmt = parse("1 + 2 * 3").unwrap();

        let Expr::Binary { lhs, op, span, rhs } = expect_expr(stmt) else {
            panic!("expected binary expression");
        };
        assert_eq!(op, BinOp::Add);
        assert_eq!(span, Span::new(0, 9));
        let Expr::Lit(Literal::Int(1), lhs_span) = *lhs else {
            panic!("left operand should be literal 1");
        };
        assert_eq!(lhs_span, Span::new(0, 1));
        let Expr::Binary {
            lhs: mul_lhs,
            op: mul_op,
            span: mul_span,
            rhs: mul_rhs,
        } = *rhs
        else {
            panic!("expected multiplication on the right");
        };
        assert_eq!(mul_op, BinOp::Mul);
        assert_eq!(mul_span, Span::new(4, 9));
        let Expr::Lit(Literal::Int(2), lhs_mul_span) = *mul_lhs else {
            panic!("left operand of multiplication should be 2");
        };
        assert_eq!(lhs_mul_span, Span::new(4, 5));
        let Expr::Lit(Literal::Int(3), rhs_mul_span) = *mul_rhs else {
            panic!("right operand of multiplication should be 3");
        };
        assert_eq!(rhs_mul_span, Span::new(8, 9));
    }

    #[test]
    fn parses_assignment_statement() {
        let stmt = parse("answer = 42").unwrap();

        let Stmt::Assignment { name, value } = stmt else {
            panic!("expected assignment statement");
        };
        assert_eq!(name, "answer");
        let Expr::Lit(Literal::Int(42), span) = value else {
            panic!("assignment value should be literal 42");
        };
        assert_eq!(span, Span::new(9, 11));
    }

    #[test]
    fn parses_function_definition_with_patterns() {
        let stmt = parse("f(x, 1) = x").unwrap();

        let Stmt::FunctionDefinition { name, arms } = stmt else {
            panic!("expected function definition");
        };
        assert_eq!(name, "f");
        assert_eq!(arms.len(), 1);
        let FuncArm { params, body } = arms.into_iter().next().unwrap();
        assert_eq!(
            params,
            vec![
                Pattern::Identifier("x".into()),
                Pattern::Lit(Literal::Int(1)),
            ]
        );
        let Expr::Identifier(name, span) = body else {
            panic!("function body should be identifier `x`");
        };
        assert_eq!(name, "x");
        assert_eq!(span, Span::new(10, 11));
    }

    #[test]
    fn reports_unclosed_grouping() {
        let err = parse("(1 + 2").unwrap_err();
        assert!(
            matches!(err, ParseError::UnexpectedToken { .. }),
            "expected unexpected token error, got {err:?}"
        );
    }

    #[test]
    fn rejects_trailing_tokens() {
        let err = parse("1 2").unwrap_err();
        assert!(
            matches!(
                err,
                ParseError::UnexpectedToken {
                    ref expected,
                    found: Some(TokenKind::Integer(2)),
                    span: SpanLabel(Some(Span { start: 2, .. }))
                } if expected == "end of input"
            ),
            "expected EOF error for trailing literal, got {err:?}"
        );

        let err = parse("f(x) y").unwrap_err();
        assert!(
            matches!(
                err,
                ParseError::UnexpectedToken {
                    ref expected,
                    found: Some(TokenKind::Identifier("y")),
                    span: SpanLabel(Some(Span { start: 5, .. }))
                } if expected == "end of input"
            ),
            "expected EOF error for trailing identifier, got {err:?}"
        );
    }

    fn expect_expr(stmt: Stmt) -> Expr {
        match stmt {
            Stmt::Expression(expr) => expr,
            other => panic!("expected expression statement, got {other:?}"),
        }
    }
}
