use std::iter::Peekable;

use crate::lexer::{
    Lexer,
    token::{Span, Token, TokenKind},
};

pub mod ast;
mod error;

use ast::*;
use error::ParseError;

/// Single-statement parser for a REPL input.
/// Wraps a `Lexer` that yields `Result<Token, LexError>` and exposes Pratt parsing.
pub struct Parser<'a> {
    /// Lookahead-capable stream of tokens (or lexer errors).
    lexer: Peekable<Lexer<'a>>,
    last_span: Option<Span>,
    last_token: Option<TokenKind<'a>>,
    source_len: usize,
}

impl<'a> Parser<'a> {
    /// Build a parser from a source string.
    pub fn new(lexer: Lexer<'a>) -> Self {
        let source_len = lexer.source_len();
        Self {
            lexer: lexer.peekable(),
            last_span: None,
            last_token: None,
            source_len,
        }
    }

    /// Entry point. Parse exactly one statement.
    pub fn parse(&mut self) -> Result<Stmt, ParseError> {
        let stmt = self.parse_stmt()?;
        if let Some(tok) = self.peek()? {
            return Err(ParseError::unexpected_token("end of input", Some(tok)));
        }
        Ok(stmt)
    }

    /// Peek next token without consuming. Propagate lexer errors.
    fn peek(&mut self) -> Result<Option<&Token<'a>>, ParseError> {
        match self.lexer.peek() {
            Some(Ok(tok)) => Ok(Some(tok)),
            Some(Err(e)) => Err(ParseError::from(e.clone())),
            None => Ok(None),
        }
    }

    /// Consume one token. Propagate lexer errors.
    fn bump(&mut self) -> Result<Option<Token<'a>>, ParseError> {
        match self.lexer.next() {
            Some(Ok(tok)) => {
                self.last_span = Some(tok.span);
                self.last_token = Some(tok.kind.clone());
                Ok(Some(tok))
            }
            Some(Err(e)) => Err(ParseError::from(e)),
            None => Ok(None),
        }
    }

    fn fallback_span(&self) -> Option<Span> {
        if let Some(span) = self.last_span {
            Some(span)
        } else if self.source_len > 0 {
            Some(Span::new(
                self.source_len.saturating_sub(1),
                self.source_len,
            ))
        } else {
            None
        }
    }

    fn fallback_found(&self) -> Option<String> {
        self.last_token.as_ref().map(|kind| kind.to_string())
    }

    fn fallback_found_and_span(&self) -> (Option<String>, Option<Span>) {
        (self.fallback_found(), self.fallback_span())
    }

    /// If next token equals `expected`, consume it and return true.
    fn eat(&mut self, expected: TokenKind<'a>) -> Result<bool, ParseError> {
        if matches!(self.peek()?, Some(t) if t.kind == expected) {
            self.bump()?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn expect_token(&mut self, expected: TokenKind<'a>) -> Result<Token<'a>, ParseError> {
        let expected_str = expected.to_string();
        match self.bump()? {
            Some(tok) if tok.kind == expected => Ok(tok),
            Some(tok) => Err(ParseError::unexpected(
                expected_str.clone(),
                Some(tok.kind.to_string()),
                Some(tok.span),
            )),
            None => {
                let (found, span) = self.fallback_found_and_span();
                Err(ParseError::unexpected(expected_str, found, span))
            }
        }
    }

    /// Require the next token to be `expected`. Error otherwise.
    fn expect(&mut self, expected: TokenKind<'a>) -> Result<(), ParseError> {
        self.expect_token(expected)?;
        Ok(())
    }

    /// stmt := func_def | assignment | expr
    fn parse_stmt(&mut self) -> Result<Stmt, ParseError> {
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
    fn parse_stmt_starting_with_ident(&mut self) -> Result<Stmt, ParseError> {
        // consume the leading name
        let (name, name_span) = match self.bump()? {
            Some(sp) => match sp.kind {
                TokenKind::Identifier(s) => (s.to_string(), sp.span),
                other => {
                    return Err(ParseError::unexpected(
                        "identifier",
                        Some(other.to_string()),
                        Some(sp.span),
                    ));
                }
            },
            None => {
                let (found, span) = self.fallback_found_and_span();
                return Err(ParseError::unexpected("identifier", found, span));
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
    fn parse_pattern_list(&mut self) -> Result<Vec<Pattern>, ParseError> {
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
    fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
        match self.bump()? {
            Some(Token { kind, span }) => match kind {
                TokenKind::Identifier(s) => Ok(Pattern::Identifier(s.to_string())),
                TokenKind::Integer(n) => Ok(Pattern::Lit(Literal::Int(n))),
                TokenKind::Float(x) => Ok(Pattern::Lit(Literal::Float(x))),
                TokenKind::Bool(b) => Ok(Pattern::Lit(Literal::Bool(b))),
                other => Err(ParseError::unexpected(
                    "literal",
                    Some(other.to_string()),
                    Some(span),
                )),
            },
            None => {
                let (found, span) = self.fallback_found_and_span();
                Err(ParseError::unexpected("literal", found, span))
            }
        }
    }

    /// Pratt parse with given minimum binding power `min_bp`.
    /// Handles:
    /// - prefix unary ops: `!` and `-`
    /// - postfix call: `expr(args...)`
    /// - binary ops with precedence/associativity from `infix_bp`
    fn parse_expr_bp(&mut self, min_bp: u8) -> Result<Expr, ParseError> {
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
            let (found, span) = self.fallback_found_and_span();
            return Err(ParseError::unexpected("expression", found, span));
        };

        self.parse_expr_bp_with_lhs(lhs, min_bp)
    }

    /// Continue Pratt parsing when the caller already parsed an initial `lhs`.
    fn parse_expr_bp_with_lhs(&mut self, mut lhs: Expr, min_bp: u8) -> Result<Expr, ParseError> {
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
    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
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
            Some(Token { kind, span }) => Err(ParseError::unexpected(
                "expression",
                Some(kind.to_string()),
                Some(span),
            )),
            None => {
                let (found, span) = self.fallback_found_and_span();
                Err(ParseError::unexpected("expression", found, span))
            }
        }
    }

    /// Lookahead from after an identifier:
    /// Return true if the next tokens are a parameter list `(...)`
    /// and the following token is `=`, indicating a function definition.
    fn lookahead_func_def_after_params(&mut self) -> Result<bool, ParseError> {
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
        for next in snap.by_ref() {
            let t = next.map_err(ParseError::from)?;
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
        TokenKind::Eq => Some((Eq, 6, 7)),
        TokenKind::Ne => Some((Ne, 6, 7)),
        TokenKind::Lt => Some((Lt, 7, 8)),
        TokenKind::LtEq => Some((LtEq, 7, 8)),
        TokenKind::Gt => Some((Gt, 7, 8)),
        TokenKind::GtEq => Some((GtEq, 7, 8)),
        TokenKind::Plus => Some((Add, 8, 9)),
        TokenKind::Minus => Some((Sub, 8, 9)),
        TokenKind::Star => Some((Mul, 9, 10)),
        TokenKind::Slash => Some((Div, 9, 10)),
        TokenKind::Percent => Some((Mod, 9, 10)),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::token::Span;

    fn parse(input: &str) -> Result<Stmt, ParseError> {
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
    fn parses_additional_binary_ops() {
        let Expr::Binary { op, span, .. } = parse_expr("1 != 2") else {
            panic!("expected binary expression");
        };
        assert_eq!(op, BinOp::Ne);
        assert_eq!(span, Span::new(0, 6));

        let Expr::Binary { op, span, .. } = parse_expr("3 < 4") else {
            panic!("expected comparison expression");
        };
        assert_eq!(op, BinOp::Lt);
        assert_eq!(span, Span::new(0, 5));

        let Expr::Binary { op, span, .. } = parse_expr("4 > 3") else {
            panic!("expected comparison expression");
        };
        assert_eq!(op, BinOp::Gt);
        assert_eq!(span, Span::new(0, 5));

        let Expr::Binary { op, span, .. } = parse_expr("5 - 2") else {
            panic!("expected subtraction expression");
        };
        assert_eq!(op, BinOp::Sub);
        assert_eq!(span, Span::new(0, 5));

        let Expr::Binary { op, span, .. } = parse_expr("2 * 3") else {
            panic!("expected multiplication expression");
        };
        assert_eq!(op, BinOp::Mul);
        assert_eq!(span, Span::new(0, 5));

        let Expr::Binary { op, span, .. } = parse_expr("8 / 4") else {
            panic!("expected division expression");
        };
        assert_eq!(op, BinOp::Div);
        assert_eq!(span, Span::new(0, 5));

        let Expr::Binary { op, span, .. } = parse_expr("9 % 5") else {
            panic!("expected modulo expression");
        };
        assert_eq!(op, BinOp::Mod);
        assert_eq!(span, Span::new(0, 5));
    }

    #[test]
    fn binop_variants_constructible() {
        use BinOp::*;

        let all = [
            And, BitAnd, Or, BitOr, Eq, Ne, Lt, LtEq, Gt, GtEq, Xor, Add, Sub, Mul, Div, Mod,
        ];
        assert_eq!(all.len(), 16);
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
        if let ParseError::UnexpectedToken {
            expected,
            found,
            span,
        } = err
        {
            assert_eq!(expected, "end of input");
            assert_eq!(found, "'2'");
            let span = span.expect("span present for trailing literal");
            let offset: usize = span.offset();
            assert_eq!(offset, 2);
        } else {
            panic!("expected EOF error for trailing literal, got {err:?}");
        }

        let err = parse("f(x) y").unwrap_err();
        if let ParseError::UnexpectedToken {
            expected,
            found,
            span,
        } = err
        {
            assert_eq!(expected, "end of input");
            assert_eq!(found, "'y'");
            let span = span.expect("span present for trailing identifier");
            let offset: usize = span.offset();
            assert_eq!(offset, 5);
        } else {
            panic!("expected EOF error for trailing identifier, got {err:?}");
        }
    }

    fn expect_expr(stmt: Stmt) -> Expr {
        match stmt {
            Stmt::Expression(expr) => expr,
            other => panic!("expected expression statement, got {other:?}"),
        }
    }

    fn parse_expr(input: &str) -> Expr {
        expect_expr(parse(input).unwrap())
    }
}
