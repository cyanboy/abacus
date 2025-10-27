use std::fmt;

#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    Identifier(&'a str),
    Literal(LiteralKind),
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

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Identifier(s) => write!(f, "{}", s),
            Token::Literal(kind) => match kind {
                LiteralKind::Bool(b) => write!(f, "{}", b),
                LiteralKind::Integer(n) => write!(f, "{}", n),
                LiteralKind::Float(n) => write!(f, "{}", n),
            },
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Star => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Percent => write!(f, "%"),
            Token::Bang => write!(f, "!"),
            Token::Comma => write!(f, ","),
            Token::Eq => write!(f, "="),
            Token::Lt => write!(f, "<"),
            Token::Gt => write!(f, ">"),
            Token::LtEq => write!(f, "<="),
            Token::GtEq => write!(f, ">="),
            Token::EqEq => write!(f, "=="),
            Token::Ne => write!(f, "!="),
            Token::OrOr => write!(f, "||"),
            Token::AndAnd => write!(f, "&&"),
            Token::OpenParen => write!(f, "("),
            Token::CloseParen => write!(f, ")"),
            Token::OpenBracket => write!(f, "["),
            Token::CloseBracket => write!(f, "]"),
            Token::Or => write!(f, "|"),
            Token::And => write!(f, "&"),
            Token::Caret => write!(f, "^"),
        }
    }
}
