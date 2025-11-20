use std::fmt;

use miette::SourceSpan;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind<'a> {
    Identifier(&'a str),
    Integer(i64),
    Float(f64),
    Bool(bool),
    Assign,     // '='
    Plus,       // '+'
    Minus,      // '-'
    Star,       // '*'
    Slash,      // '/'
    Percent,    // '%'
    Bang,       // '!'
    Caret,      // '^'
    Eq,         // '=='
    Gt,         // '>'
    GtEq,       // '>='
    Lt,         // '<'
    LtEq,       // '<='
    Ne,         // '!='
    BitOr,      // '|'
    Or,         // '||'
    BitAnd,     // '&'
    And,        // '&&'
    Comma,      // ','
    OpenParen,  // '('
    CloseParen, // ')'
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub const fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub const fn is_empty(&self) -> bool {
        self.start == self.end
    }

    pub fn len(&self) -> usize {
        self.end.saturating_sub(self.start)
    }

    pub fn into_source_span(self) -> SourceSpan {
        let len = self.len().max(1);
        SourceSpan::new(self.start.into(), len)
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
    pub span: Span,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenKind<'a>, span: Span) -> Self {
        Self { kind, span }
    }
}

impl fmt::Display for TokenKind<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TokenKind::*;
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
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn span_into_source_span_preserves_offsets() {
        let span = Span::new(2, 5);
        let source_span = span.into_source_span();
        let offset: usize = source_span.offset();
        assert_eq!(offset, 2);
        assert_eq!(source_span.len(), 3);
    }
}
