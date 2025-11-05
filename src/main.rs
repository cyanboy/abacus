use miette::{GraphicalReportHandler, NamedSource, Report};
use rustyline::DefaultEditor;
use unicode_width::UnicodeWidthStr;

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
                            print_report(&message, input, report);
                        }
                    },
                    Err(e) => {
                        let message = e.to_string();
                        let report = Report::new(e)
                            .with_source_code(NamedSource::new("<repl>", input.to_string()));
                        print_report(&message, input, report);
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

fn print_report(message: &str, source: &str, report: Report) {
    eprintln!("Error: {message}");

    let handler = GraphicalReportHandler::new().without_cause_chain();
    let mut rendered = String::new();
    if handler
        .render_report(&mut rendered, report.as_ref())
        .is_err()
    {
        render_fallback(source, &report);
        return;
    }

    let output_lines: Vec<&str> = rendered
        .lines()
        .skip_while(|line| {
            let trimmed = line.trim();
            trimmed.is_empty() || trimmed.ends_with(message)
        })
        .collect();

    if output_lines.is_empty() {
        render_fallback(source, &report);
        return;
    }

    eprintln!();
    eprintln!("{}", output_lines.join("\n"));
}

fn render_fallback(source: &str, report: &Report) {
    if source.is_empty() {
        return;
    }

    let label_offset = report
        .labels()
        .and_then(|labels| {
            let labels: Vec<_> = labels.collect();
            labels
                .iter()
                .find(|label| label.primary())
                .or_else(|| labels.first())
                .map(|label| label.offset())
        })
        .unwrap_or_else(|| source.len());
    let byte_index = label_offset.min(source.len());
    let prefix = &source[..byte_index];
    let caret_pad = " ".repeat(UnicodeWidthStr::width(prefix));

    eprintln!();
    eprintln!("  1 | {source}");
    eprintln!("    | {caret_pad}^");
}
