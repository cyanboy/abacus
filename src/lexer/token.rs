use std::fmt;

#[derive(Debug, PartialEq)]
pub enum TokenKind<'a> {
    Identifier(&'a str),
    Literal(LiteralKind),
    // Int(i64),
    // Float(f64),
    // Bool(bool),
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Bang,
    Comma,
    Caret,
    Eq,
    Gt,
    GtEq,
    Lt,
    LtEq,
    EqEq,
    Ne,
    Or,
    OrOr,
    And,
    AndAnd,
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
}

#[derive(Debug, PartialEq)]
pub enum LiteralKind {
    Bool(bool),
    Integer(i64),
    Float(f64),
}

impl fmt::Display for TokenKind<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::Identifier(s) => write!(f, "{}", s),
            TokenKind::Literal(kind) => match kind {
                LiteralKind::Bool(b) => write!(f, "{}", b),
                LiteralKind::Integer(n) => write!(f, "{}", n),
                LiteralKind::Float(n) => write!(f, "{}", n),
            },
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Star => write!(f, "*"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::Percent => write!(f, "%"),
            TokenKind::Bang => write!(f, "!"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Eq => write!(f, "="),
            TokenKind::Lt => write!(f, "<"),
            TokenKind::Gt => write!(f, ">"),
            TokenKind::LtEq => write!(f, "<="),
            TokenKind::GtEq => write!(f, ">="),
            TokenKind::EqEq => write!(f, "=="),
            TokenKind::Ne => write!(f, "!="),
            TokenKind::OrOr => write!(f, "||"),
            TokenKind::AndAnd => write!(f, "&&"),
            TokenKind::OpenParen => write!(f, "("),
            TokenKind::CloseParen => write!(f, ")"),
            TokenKind::OpenBracket => write!(f, "["),
            TokenKind::CloseBracket => write!(f, "]"),
            TokenKind::Or => write!(f, "|"),
            TokenKind::And => write!(f, "&"),
            TokenKind::Caret => write!(f, "^"),
        }
    }
}
