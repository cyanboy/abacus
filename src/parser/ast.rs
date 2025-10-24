#[derive(Debug, PartialEq)]
pub enum Statement<'a> {
    Assignment {
        name: &'a str,
        value: Expression<'a>,
    },
    FunctionDefinition {
        name: &'a str,
        params: Vec<Pattern<'a>>,
        body: Expression<'a>,
    },
    Expression(Expression<'a>),
}

#[derive(Debug, PartialEq)]
pub enum Expression<'a> {
    Identifier(&'a str),
    Int(i32),
    Bool(bool),
    BinaryOp {
        left: Box<Expression<'a>>,
        op: BinaryOperator,
        right: Box<Expression<'a>>,
    },
    UnaryOp {
        op: UnaryOperator,
        operand: Box<Expression<'a>>,
    },
    FunctionCall {
        name: &'a str,
        args: Vec<Expression<'a>>,
    },
}

#[derive(Debug, PartialEq)]
pub enum Pattern<'a> {
    Identifier(&'a str),
    Int(i32),
    Bool(bool),
    Wildcard,
}

#[derive(Debug, PartialEq)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    BitAnd,
    BitOr,
    BitXor,
    And,
    Or,
    Eq,
    NotEq,
    Lt,
    Gt,
    LtEq,
    GtEq,
}

#[derive(Debug, PartialEq)]
pub enum UnaryOperator {
    Not,
    Negate,
}

#[cfg(test)]
mod tests {}
