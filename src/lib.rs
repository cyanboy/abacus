pub mod colors;
pub mod interpreter;
pub mod lexer;
pub mod parser;
pub mod repl;

pub mod runner;

pub use runner::{RunConfig, run, run_expression, run_file, run_with_config};
