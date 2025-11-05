use miette::{GraphicalReportHandler, NamedSource, Report};
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
                            let message = e.to_string();
                            let report = Report::new(e)
                                .with_source_code(NamedSource::new("<repl>", input.to_string()));
                            print_report(&message, report);
                        }
                    },
                    Err(e) => {
                        let message = e.to_string();
                        let report = Report::new(e)
                            .with_source_code(NamedSource::new("<repl>", input.to_string()));
                        print_report(&message, report);
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

fn print_report(message: &str, report: Report) {
    eprintln!("Error: {message}");

    let handler = GraphicalReportHandler::new().without_cause_chain();
    let mut rendered = String::new();
    if handler
        .render_report(&mut rendered, report.as_ref())
        .is_err()
    {
        return;
    }

    let mut output_lines = Vec::new();
    let mut skipped_header = false;
    for line in rendered.lines() {
        let trimmed = line.trim();
        if !skipped_header {
            if trimmed.is_empty() {
                continue;
            }
            if trimmed.ends_with(message) {
                skipped_header = true;
                continue;
            }
        }
        output_lines.push(line);
    }

    if output_lines.is_empty() {
        return;
    }

    eprintln!();
    eprintln!("{}", output_lines.join("\n"));
}
