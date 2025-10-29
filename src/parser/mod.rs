use std::iter::Peekable;

use crate::lexer::{
    Lexer,
    token::{Token, TokenKind},
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

    /// Require the next token to be `expected`. Error otherwise.
    fn expect(&mut self, expected: TokenKind<'a>) -> Result<(), ParseError<'a>> {
        match self.bump()? {
            Some(t) if t.kind == expected => Ok(()),
            found => {
                let (tok, span): (Option<_>, SpanLabel) = match found {
                    Some(sp) => (Some(sp.kind), Some(sp.span).into()),
                    None => (None, None.into()),
                };
                Err(ParseError::UnexpectedToken {
                    expected: expected.to_string(),
                    found: tok,
                    span,
                })
            }
        }
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
        let (name, _name_span) = match self.bump()? {
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
        let lhs = Expr::Identifier(name);
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
        // Parse prefix or primary.
        let mut lhs = if let Some(op) = self.peek()?.and_then(|t| prefix_op(&t.kind)) {
            // consume unary op, then parse a tightly-binding RHS
            self.bump()?;
            let rhs = self.parse_expr_bp(PREFIX_BP)?;
            Expr::Unary {
                op,
                rhs: Box::new(rhs),
            }
        } else {
            self.parse_primary()?
        };

        // Loop for postfix and infix operators.
        loop {
            // Postfix call has highest precedence.
            if self.eat(TokenKind::OpenParen)? {
                let mut args = Vec::new();
                if !self.eat(TokenKind::CloseParen)? {
                    loop {
                        args.push(self.parse_expr_bp(0)?);
                        if self.eat(TokenKind::Comma)? {
                            continue;
                        }
                        self.expect(TokenKind::CloseParen)?;
                        break;
                    }
                }
                lhs = Expr::Call {
                    callee: Box::new(lhs),
                    args,
                };
                continue;
            }

            // Decide next binary operator and its binding powers.
            let (op, lbp, rbp) = match self.peek()?.and_then(|t| infix_bp(&t.kind)) {
                Some(x) => x,
                None => break,
            };
            if lbp < min_bp {
                break;
            }

            // Consume operator and parse the RHS at `rbp`.
            self.bump()?;
            let rhs = self.parse_expr_bp(rbp)?;
            lhs = Expr::Binary {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            };
        }

        Ok(lhs)
    }

    /// Continue Pratt parsing when the caller already parsed an initial `lhs`.
    fn parse_expr_bp_with_lhs(
        &mut self,
        mut lhs: Expr,
        min_bp: u8,
    ) -> Result<Expr, ParseError<'a>> {
        loop {
            // Postfix call
            if self.eat(TokenKind::OpenParen)? {
                let mut args = Vec::new();
                if !self.eat(TokenKind::CloseParen)? {
                    loop {
                        args.push(self.parse_expr_bp(0)?);
                        if self.eat(TokenKind::Comma)? {
                            continue;
                        }
                        self.expect(TokenKind::CloseParen)?;
                        break;
                    }
                }
                lhs = Expr::Call {
                    callee: Box::new(lhs),
                    args,
                };
                continue;
            }

            // Infix
            let (op, lbp, rbp) = match self.peek()?.and_then(|t| infix_bp(&t.kind)) {
                Some(x) => x,
                None => break,
            };
            if lbp < min_bp {
                break;
            }

            self.bump()?;
            let rhs = self.parse_expr_bp(rbp)?;
            lhs = Expr::Binary {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            };
        }
        Ok(lhs)
    }

    /// primary := literal | identifier | '(' expr ')'
    fn parse_primary(&mut self) -> Result<Expr, ParseError<'a>> {
        Ok(match self.bump()? {
            Some(Token {
                kind: TokenKind::Integer(n),
                ..
            }) => Expr::Lit(Literal::Int(n)),
            Some(Token {
                kind: TokenKind::Float(x),
                ..
            }) => Expr::Lit(Literal::Float(x)),
            Some(Token {
                kind: TokenKind::Bool(b),
                ..
            }) => Expr::Lit(Literal::Bool(b)),
            Some(Token {
                kind: TokenKind::Identifier(s),
                ..
            }) => Expr::Identifier(s.to_string()),
            Some(Token {
                kind: TokenKind::OpenParen,
                ..
            }) => {
                let e = self.parse_expr_bp(0)?;
                self.expect(TokenKind::CloseParen)?;
                Expr::Group(Box::new(e))
            }
            found => {
                let (tok, span): (Option<_>, SpanLabel) = match found {
                    Some(t) => (Some(t.kind), Some(t.span).into()),
                    None => (None, None.into()),
                };
                return Err(ParseError::UnexpectedToken {
                    expected: "expression".to_string(),
                    found: tok,
                    span,
                });
            }
        })
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
    let la = |op, p| Some((op, p, p + 1));

    match tok {
        TokenKind::Star => la(Mul, 90),
        TokenKind::Slash => la(Div, 90),
        TokenKind::Percent => la(Mod, 90),
        TokenKind::Plus => la(Add, 80),
        TokenKind::Minus => la(Sub, 80),
        TokenKind::Caret => la(Xor, 70),
        TokenKind::BitAnd => la(BitAnd, 65),
        TokenKind::BitOr => la(BitOr, 60),
        TokenKind::Eq => la(Eq, 50),
        TokenKind::Ne => la(Ne, 50),
        TokenKind::Lt => la(Lt, 50),
        TokenKind::LtEq => la(LtEq, 50),
        TokenKind::Gt => la(Gt, 50),
        TokenKind::GtEq => la(GtEq, 50),
        TokenKind::And => la(And, 40),
        TokenKind::Or => la(Or, 30),

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

        let Expr::Binary { lhs, op, rhs } = expect_expr(stmt) else {
            panic!("expected binary expression");
        };
        assert_eq!(op, BinOp::Add);
        assert_eq!(
            *lhs,
            Expr::Lit(Literal::Int(1)),
            "left operand should be literal 1"
        );
        let Expr::Binary {
            lhs: mul_lhs,
            op: mul_op,
            rhs: mul_rhs,
        } = *rhs
        else {
            panic!("expected multiplication on the right");
        };
        assert_eq!(mul_op, BinOp::Mul);
        assert_eq!(
            *mul_lhs,
            Expr::Lit(Literal::Int(2)),
            "left operand of multiplication should be 2"
        );
        assert_eq!(
            *mul_rhs,
            Expr::Lit(Literal::Int(3)),
            "right operand of multiplication should be 3"
        );
    }

    #[test]
    fn parses_assignment_statement() {
        let stmt = parse("answer = 42").unwrap();

        let Stmt::Assignment { name, value } = stmt else {
            panic!("expected assignment statement");
        };
        assert_eq!(name, "answer");
        assert_eq!(
            value,
            Expr::Lit(Literal::Int(42)),
            "assignment value should parse as literal 42"
        );
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
        assert_eq!(
            body,
            Expr::Identifier("x".into()),
            "function body should be identifier `x`"
        );
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
