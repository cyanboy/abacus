use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'a> {
    Identifier(&'a str),
    Integer(i64),
    Float(f64),
    Bool(bool),
    Assign,       // '='
    Plus,         // '+'
    Minus,        // '-'
    Star,         // '*'
    Slash,        // '/'
    Percent,      // '%'
    Bang,         // '!'
    Caret,        // '^'
    Eq,           // '=='
    Gt,           // '>'
    GtEq,         // '>='
    Lt,           // '<'
    LtEq,         // '<='
    Ne,           // '!='
    BitOr,        // '|'
    Or,           // '||'
    BitAnd,       // '&'
    And,          // '&&'
    Comma,        // ','
    OpenParen,    // '('
    CloseParen,   // ')
    OpenBracket,  // '['
    CloseBracket, // ']'
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Token::*;
        match self {
            Identifier(id) => write!(f, "{}", id),
            Integer(num) => write!(f, "{}", num),
            Float(num) => write!(f, "{}", num),
            Bool(val) => write!(f, "{}", val),
            Assign => write!(f, "="),
            Plus => write!(f, "+"),
            Minus => write!(f, "-"),
            Star => write!(f, "*"),
            Slash => write!(f, "/"),
            Percent => write!(f, "%"),
            Bang => write!(f, "!"),
            Caret => write!(f, "^"),
            Eq => write!(f, "=="),
            Gt => write!(f, ">"),
            GtEq => write!(f, ">="),
            Lt => write!(f, "<"),
            LtEq => write!(f, "<="),
            Ne => write!(f, "!="),
            BitOr => write!(f, "|"),
            Or => write!(f, "||"),
            BitAnd => write!(f, "&"),
            And => write!(f, "&&"),
            Comma => write!(f, ","),
            OpenParen => write!(f, "("),
            CloseParen => write!(f, ")"),
            OpenBracket => write!(f, "["),
            CloseBracket => write!(f, "]"),
        }
    }
}
