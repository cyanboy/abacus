pub mod ast;
mod error;

pub use error::ParseError;

use crate::{Statement, lexer::Lexer};

pub struct Parser<'a> {
    _lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(s: &'a str) -> Self {
        Self {
            _lexer: Lexer::new(s),
        }
    }

    pub fn parse(&self) -> Result<Vec<Statement<'_>>, ParseError<'_>> {
        todo!("implement parse()")
    }
}
