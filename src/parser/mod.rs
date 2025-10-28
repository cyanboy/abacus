use std::iter::Peekable;

use crate::lexer::{
    Lexer,
    token::{LiteralKind, OperatorKind, SeparatorKind, Token},
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
}

impl<'a> Parser<'a> {
    /// Build a parser from a source string.
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            lexer: lexer.peekable(),
        }
    }

    /// Entry point. Parse exactly one statement.
    pub fn parse(&mut self) -> Result<Stmt, ParseError<'_>> {
        self.parse_stmt()
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
    fn eat(&mut self, expected: &Token<'a>) -> Result<bool, ParseError<'a>> {
        if matches!(self.peek()?, Some(t) if t == expected) {
            self.bump()?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    /// Require the next token to be `expected`. Error otherwise.
    fn expect(&mut self, expected: &Token<'a>) -> Result<(), ParseError<'a>> {
        match self.bump()? {
            Some(t) if &t == expected => Ok(()),
            found => Err(ParseError::UnexpectedToken {
                expected: expected.to_string(),
                found,
            }),
        }
    }

    /// Shorthand constructors to reduce noise at call sites.
    #[inline]
    fn separator(&self, k: SeparatorKind) -> Token<'a> {
        Token::Separator(k)
    }
    #[inline]
    fn operator(&self, k: OperatorKind) -> Token<'a> {
        Token::Operator(k)
    }

    /// stmt := func_def | assignment | expr
    fn parse_stmt(&mut self) -> Result<Stmt, ParseError<'a>> {
        match self.peek()? {
            Some(Token::Identifier(_)) => self.parse_stmt_starting_with_ident(),
            _ => Ok(Stmt::Expression(self.parse_expr_bp(0)?)),
        }
    }

    /// Disambiguate after seeing a leading identifier:
    /// - `f(<patterns>) = <expr>` → function definition
    /// - `<name> = <expr>` → assignment
    /// - otherwise treat the identifier as the start of an expression
    fn parse_stmt_starting_with_ident(&mut self) -> Result<Stmt, ParseError<'a>> {
        // consume the leading name
        let name = match self.bump()? {
            Some(Token::Identifier(s)) => s.to_string(),
            found => {
                return Err(ParseError::UnexpectedToken {
                    expected: "identifier".into(),
                    found,
                });
            }
        };

        // Function definition only if we see "( ... )" followed by '='.
        if matches!(
            self.peek()?,
            Some(Token::Separator(SeparatorKind::OpenParen))
        ) && self.lookahead_func_def_after_params()?
        {
            self.expect(&self.separator(SeparatorKind::OpenParen))?;
            let params = self.parse_pattern_list()?;
            self.expect(&self.operator(OperatorKind::Assign))?;
            let body = self.parse_expr_bp(0)?;
            return Ok(Stmt::FunctionDefinition {
                name,
                arms: vec![FuncArm { params, body }],
            });
        }

        // Assignment: name '=' expr
        if self.eat(&self.operator(OperatorKind::Assign))? {
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
        if self.eat(&self.separator(SeparatorKind::CloseParen))? {
            return Ok(params);
        }
        // One or more patterns separated by commas.
        loop {
            params.push(self.parse_pattern()?);
            if self.eat(&self.separator(SeparatorKind::Comma))? {
                continue;
            }
            self.expect(&self.separator(SeparatorKind::CloseParen))?;
            break;
        }
        Ok(params)
    }

    /// pattern := identifier | literal
    fn parse_pattern(&mut self) -> Result<Pattern, ParseError<'a>> {
        match self.bump()? {
            Some(Token::Identifier(s)) => Ok(Pattern::Identifier(s.to_string())),
            Some(Token::Literal(LiteralKind::Integer(n))) => Ok(Pattern::Lit(Literal::Int(n))),
            Some(Token::Literal(LiteralKind::Float(x))) => Ok(Pattern::Lit(Literal::Float(x))),
            Some(Token::Literal(LiteralKind::Bool(b))) => Ok(Pattern::Lit(Literal::Bool(b))),
            found => Err(ParseError::UnexpectedToken {
                expected: "literal".to_string(),
                found,
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
        let mut lhs = if let Some(op) = self.peek()?.and_then(prefix_op) {
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
            if self.eat(&self.separator(SeparatorKind::OpenParen))? {
                let mut args = Vec::new();
                if !self.eat(&self.separator(SeparatorKind::CloseParen))? {
                    loop {
                        args.push(self.parse_expr_bp(0)?);
                        if self.eat(&self.separator(SeparatorKind::Comma))? {
                            continue;
                        }
                        self.expect(&self.separator(SeparatorKind::CloseParen))?;
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
            let (op, lbp, rbp) = match self.peek()?.and_then(infix_bp) {
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
            if self.eat(&self.separator(SeparatorKind::OpenParen))? {
                let mut args = Vec::new();
                if !self.eat(&self.separator(SeparatorKind::CloseParen))? {
                    loop {
                        args.push(self.parse_expr_bp(0)?);
                        if self.eat(&self.separator(SeparatorKind::Comma))? {
                            continue;
                        }
                        self.expect(&self.separator(SeparatorKind::CloseParen))?;
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
            let (op, lbp, rbp) = match self.peek()?.and_then(infix_bp) {
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
            Some(Token::Literal(LiteralKind::Integer(n))) => Expr::Lit(Literal::Int(n)),
            Some(Token::Literal(LiteralKind::Float(x))) => Expr::Lit(Literal::Float(x)),
            Some(Token::Literal(LiteralKind::Bool(b))) => Expr::Lit(Literal::Bool(b)),
            Some(Token::Identifier(s)) => Expr::Identifier(s.to_string()),
            Some(Token::Separator(SeparatorKind::OpenParen)) => {
                let e = self.parse_expr_bp(0)?;
                self.expect(&self.separator(SeparatorKind::CloseParen))?;
                Expr::Group(Box::new(e))
            }
            found => {
                return Err(ParseError::UnexpectedToken {
                    expected: "expression".to_string(),
                    found,
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
            Some(Ok(Token::Separator(SeparatorKind::OpenParen))) => {}
            _ => return Ok(false),
        }
        // scan to matching ')'
        let mut depth = 1usize;
        while let Some(next) = snap.next() {
            let t = next.map_err(ParseError::LexerError)?;
            match t {
                Token::Separator(SeparatorKind::OpenParen) => depth += 1,
                Token::Separator(SeparatorKind::CloseParen) => {
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
            Some(Ok(Token::Operator(OperatorKind::Assign))) => Ok(true),
            _ => Ok(false),
        }
    }
}

/// Binding power for prefix operators. Must bind tighter than `* / %`.
const PREFIX_BP: u8 = 100;

/// Recognize prefix unary operators.
fn prefix_op(tok: &Token) -> Option<UnaryOp> {
    match tok {
        Token::Operator(OperatorKind::Minus) => Some(UnaryOp::Neg),
        Token::Operator(OperatorKind::Bang) => Some(UnaryOp::Not),
        _ => None,
    }
}

/// Map a token to `(op, left_bp, right_bp)`.
/// Left-associative operators use `rbp = lbp + 1`.
fn infix_bp(tok: &Token) -> Option<(BinOp, u8, u8)> {
    use BinOp::*;
    let la = |op, p| Some((op, p, p + 1));

    match tok {
        Token::Operator(OperatorKind::Star) => la(Mul, 90),
        Token::Operator(OperatorKind::Slash) => la(Div, 90),
        Token::Operator(OperatorKind::Percent) => la(Mod, 90),

        Token::Operator(OperatorKind::Plus) => la(Add, 80),
        Token::Operator(OperatorKind::Minus) => la(Sub, 80),

        Token::Operator(OperatorKind::Caret) => la(Xor, 70),
        Token::Operator(OperatorKind::BitAnd) => la(BitAnd, 65),
        Token::Operator(OperatorKind::BitOr) => la(BitOr, 60),

        Token::Operator(OperatorKind::Eq) => la(Eq, 50),
        Token::Operator(OperatorKind::Ne) => la(Ne, 50),
        Token::Operator(OperatorKind::Lt) => la(Lt, 50),
        Token::Operator(OperatorKind::LtEq) => la(LtEq, 50),
        Token::Operator(OperatorKind::Gt) => la(Gt, 50),
        Token::Operator(OperatorKind::GtEq) => la(GtEq, 50),

        Token::Operator(OperatorKind::And) => la(And, 40),
        Token::Operator(OperatorKind::Or) => la(Or, 30),

        _ => None,
    }
}

#[cfg(test)]
mod tests {}
