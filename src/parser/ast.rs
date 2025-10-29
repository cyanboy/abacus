use crate::lexer::token::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Assignment { name: String, value: Expr },
    FunctionDefinition { name: String, arms: Vec<FuncArm> },
    Expression(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Lit(Literal, Span),
    Identifier(String, Span),
    Unary {
        op: UnaryOp,
        span: Span,
        rhs: Box<Expr>,
    },
    Binary {
        lhs: Box<Expr>,
        op: BinOp,
        span: Span,
        rhs: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
        span: Span,
    },
    Group(Box<Expr>, Span),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinOp {
    And,
    BitAnd,
    Or,
    BitOr,
    Eq,
    Ne,
    Lt,
    LtEq,
    Gt,
    GtEq,
    Xor,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Identifier(String),
    Lit(Literal),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FuncArm {
    pub params: Vec<Pattern>,
    pub body: Expr,
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Lit(_, span) => *span,
            Expr::Identifier(_, span) => *span,
            Expr::Unary { span, .. } => *span,
            Expr::Binary { span, .. } => *span,
            Expr::Call { span, .. } => *span,
            Expr::Group(_, span) => *span,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn expr_span_matches_literal() {
        let span = Span::new(0, 1);
        let expr = Expr::Lit(Literal::Int(1), span);
        assert_eq!(expr.span(), span);
    }

    #[test]
    fn expr_span_matches_identifier() {
        let span = Span::new(2, 5);
        let expr = Expr::Identifier("foo".into(), span);
        assert_eq!(expr.span(), span);
    }

    #[test]
    fn expr_span_matches_unary() {
        let span = Span::new(0, 2);
        let expr = Expr::Unary {
            op: UnaryOp::Neg,
            span,
            rhs: Box::new(Expr::Lit(Literal::Int(1), Span::new(1, 2))),
        };
        assert_eq!(expr.span(), span);
    }

    #[test]
    fn expr_span_matches_binary() {
        let span = Span::new(0, 3);
        let expr = Expr::Binary {
            lhs: Box::new(Expr::Lit(Literal::Int(1), Span::new(0, 1))),
            op: BinOp::Add,
            span,
            rhs: Box::new(Expr::Lit(Literal::Int(2), Span::new(2, 3))),
        };
        assert_eq!(expr.span(), span);
    }

    #[test]
    fn expr_span_matches_call() {
        let span = Span::new(0, 4);
        let expr = Expr::Call {
            callee: Box::new(Expr::Identifier("f".into(), Span::new(0, 1))),
            args: vec![Expr::Lit(Literal::Int(1), Span::new(2, 3))],
            span,
        };
        assert_eq!(expr.span(), span);
    }

    #[test]
    fn expr_span_matches_group() {
        let span = Span::new(0, 4);
        let expr = Expr::Group(Box::new(Expr::Lit(Literal::Int(1), Span::new(1, 2))), span);
        assert_eq!(expr.span(), span);
    }
}
