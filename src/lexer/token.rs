use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'a> {
    Identifier(&'a str),
    Literal(LiteralKind),
    Operator(OperatorKind),
    Separator(SeparatorKind),
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralKind {
    Bool(bool),
    Integer(i64),
    Float(f64),
}

#[derive(Debug, Clone, PartialEq)]
pub enum OperatorKind {
    Assign,  // '='
    Plus,    // '+'
    Minus,   // '-'
    Star,    // '*'
    Slash,   // '/'
    Percent, // '%'
    Bang,    // '!'
    Caret,   // '^'
    Eq,      // '=='
    Gt,      // '>'
    GtEq,    // '>='
    Lt,      // '<'
    LtEq,    // '<='
    Ne,      // '!='
    BitOr,   // '|'
    Or,      // '||'
    BitAnd,  // '&'
    And,     // '&&'
}

#[derive(Debug, Clone, PartialEq)]
pub enum SeparatorKind {
    Comma,
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
}

impl fmt::Display for OperatorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use OperatorKind::*;
        f.write_str(match self {
            Assign => "=",
            Plus => "+",
            Minus => "-",
            Star => "*",
            Slash => "/",
            Percent => "%",
            Bang => "!",
            Caret => "^",
            Eq => "==",
            Lt => "<",
            Gt => ">",
            LtEq => "<=",
            GtEq => ">=",
            Ne => "!=",
            BitOr => "|",
            Or => "||",
            BitAnd => "&",
            And => "&&",
        })
    }
}

impl fmt::Display for SeparatorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use SeparatorKind::*;
        f.write_str(match self {
            Comma => ",",
            OpenParen => "(",
            CloseParen => ")",
            OpenBracket => "[",
            CloseBracket => "]",
        })
    }
}

impl fmt::Display for LiteralKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LiteralKind::Bool(b) => write!(f, "{b}"),
            LiteralKind::Integer(n) => write!(f, "{n}"),
            LiteralKind::Float(n) => write!(f, "{n}"),
        }
    }
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Token::*;
        match self {
            Identifier(s) => write!(f, "{s}"),
            Literal(kind) => write!(f, "{kind}"),
            Operator(kind) => write!(f, "{kind}"),
            Separator(kind) => write!(f, "{kind}"),
        }
    }
}
