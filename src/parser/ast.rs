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
mod tests {}
