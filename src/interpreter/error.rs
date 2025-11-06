use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use crate::lexer::token::Span;

#[derive(Debug, Error, Diagnostic)]
pub enum EvalError {
    #[error("undefined variable: {name}")]
    UndefinedVar {
        name: String,
        #[label("not defined here")]
        span: Option<SourceSpan>,
    },

    #[error("undefined function: {name}")]
    UndefinedFunc {
        name: String,
        #[label("not defined here")]
        span: Option<SourceSpan>,
    },

    #[error("no matching arm for function: {name}")]
    NoMatchingArm {
        name: String,
        #[label("no matching arm")]
        span: Option<SourceSpan>,
    },

    #[error("type error: {message}")]
    TypeError {
        message: &'static str,
        #[label("type error")]
        span: Option<SourceSpan>,
    },

    #[error("division by zero")]
    DivideByZero {
        #[label("division by zero")]
        span: Option<SourceSpan>,
    },
}

impl EvalError {
    pub fn undefined_var(name: String, span: Span) -> Self {
        Self::UndefinedVar {
            name,
            span: Some(span.into_source_span()),
        }
    }

    pub fn undefined_func(name: String, span: Span) -> Self {
        Self::UndefinedFunc {
            name,
            span: Some(span.into_source_span()),
        }
    }

    pub fn no_matching_arm(name: String, span: Span) -> Self {
        Self::NoMatchingArm {
            name,
            span: Some(span.into_source_span()),
        }
    }

    pub fn type_error(message: &'static str, span: Span) -> Self {
        Self::TypeError {
            message,
            span: Some(span.into_source_span()),
        }
    }

    pub fn divide_by_zero(span: Span) -> Self {
        Self::DivideByZero {
            span: Some(span.into_source_span()),
        }
    }

    pub fn with_span(self, span: Span) -> Self {
        let span = Some(span.into_source_span());
        match self {
            EvalError::UndefinedVar { name, .. } => EvalError::UndefinedVar { name, span },
            EvalError::UndefinedFunc { name, .. } => EvalError::UndefinedFunc { name, span },
            EvalError::NoMatchingArm { name, .. } => EvalError::NoMatchingArm { name, span },
            EvalError::TypeError { message, .. } => EvalError::TypeError { message, span },
            EvalError::DivideByZero { .. } => EvalError::DivideByZero { span },
        }
    }
}
