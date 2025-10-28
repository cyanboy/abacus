use thiserror::Error;

#[derive(Debug, Error)]
pub enum EvalError {
    #[error("undefined variable: {0}")]
    UndefinedVar(String),

    #[error("undefined function: {0}")]
    UndefinedFunc(String),

    #[error("no matching arm for function: {0}")]
    NoMatchingArm(String),

    #[error("arity mismatch: expected {expected}, got {got}")]
    Arity { expected: usize, got: usize },

    #[error("type error: {0}")]
    Type(&'static str),

    #[error("division by zero")]
    DivByZero,
}
