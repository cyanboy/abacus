use miette::{NamedSource, Report};
use rustyline::DefaultEditor;

mod eval;
mod lexer;
mod parser;

use crate::{eval::Env, lexer::Lexer, parser::Parser};

fn main() {
    println!("Abacus - Calculator REPL");
    println!("Type expressions or 'quit' to exit\n");

    let mut rl = DefaultEditor::new().unwrap();

    let mut env = Env::new();

    loop {
        match rl.readline("> ") {
            Ok(line) => {
                let input = line.trim();
                if input.is_empty() {
                    continue;
                }
                rl.add_history_entry(input).unwrap();

                if input == "quit" || input == "exit" {
                    println!("Goodbye!");
                    break;
                }

                let mut parser = Parser::new(Lexer::new(input));
                match parser.parse() {
                    Ok(stmt) => match env.eval_stmt(&stmt) {
                        Ok(Some(v)) => println!("{v}"),
                        Ok(None) => {}
                        Err(e) => {
                            let report = Report::new(e)
                                .with_source_code(NamedSource::new("<repl>", input.to_string()));
                            eprintln!("{report:?}");
                        }
                    },
                    Err(e) => {
                        let report = Report::new(e)
                            .with_source_code(NamedSource::new("<repl>", input.to_string()));
                        eprintln!("{report:?}");
                    }
                }
            }
            Err(_) => {
                println!("Goodbye!");
                break;
            }
        }
    }
}
