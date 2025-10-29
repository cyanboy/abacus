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
    Lit(Literal),
    Identifier(String),
    Unary {
        op: UnaryOp,
        rhs: Box<Expr>,
    },
    Binary {
        lhs: Box<Expr>,
        op: BinOp,
        rhs: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
    },
    Group(Box<Expr>),
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

#[cfg(test)]
mod tests {}
