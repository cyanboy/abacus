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

    #[test]
    fn span_len_and_empty_behave() {
        let empty = Span::new(4, 4);
        assert!(empty.is_empty());
        assert_eq!(empty.len(), 0);

        let non_empty = Span::new(1, 6);
        assert!(!non_empty.is_empty());
        assert_eq!(non_empty.len(), 5);
    }

    #[test]
    fn span_display_renders_range() {
        let span = Span::new(3, 7);
        assert_eq!(span.to_string(), "3..7");
    }

    #[test]
    fn token_kind_display_matches_source_lexeme() {
        let cases = [
            (TokenKind::Identifier("foo"), "foo"),
            (TokenKind::Integer(42), "42"),
            (TokenKind::Float(1.5), "1.5"),
            (TokenKind::Bool(true), "true"),
            (TokenKind::Assign, "="),
            (TokenKind::Plus, "+"),
            (TokenKind::Minus, "-"),
            (TokenKind::Star, "*"),
            (TokenKind::Slash, "/"),
            (TokenKind::Percent, "%"),
            (TokenKind::Bang, "!"),
            (TokenKind::Caret, "^"),
            (TokenKind::Eq, "=="),
            (TokenKind::Gt, ">"),
            (TokenKind::GtEq, ">="),
            (TokenKind::Lt, "<"),
            (TokenKind::LtEq, "<="),
            (TokenKind::Ne, "!="),
            (TokenKind::BitOr, "|"),
            (TokenKind::Or, "||"),
            (TokenKind::BitAnd, "&"),
            (TokenKind::And, "&&"),
            (TokenKind::Comma, ","),
            (TokenKind::OpenParen, "("),
            (TokenKind::CloseParen, ")"),
        ];
        for (kind, expected) in cases {
            assert_eq!(kind.to_string(), expected);
        }
    }

    #[test]
    fn span_into_source_span_expands_empty_to_single_byte() {
        let span = Span::new(5, 5);
        let src = span.into_source_span();
        assert_eq!(src.offset(), 5);
        assert_eq!(src.len(), 1);
    }
}
