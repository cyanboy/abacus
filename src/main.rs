use miette::{NamedSource, Report};

mod eval;
mod lexer;
mod parser;
mod repl;

use crate::{
    eval::Env,
    lexer::Lexer,
    parser::Parser,
    repl::{create_editor, format_value, print_report},
};

const TITLE: &str = "Abacus - Calculator REPL";
const TITLE_COLORS: &[&str] = &[
    "\x1b[31m", "\x1b[33m", "\x1b[32m", "\x1b[36m", "\x1b[34m", "\x1b[35m",
];
const COLOR_RESET: &str = "\x1b[0m";
const COLOR_BOLD: &str = "\x1b[1m";

fn main() {
    println!("{}", colorize_title(TITLE));
    println!("Type expressions or 'quit' to exit\n");

    let mut rl = create_editor().expect("failed to initialize REPL editor");
    let mut env = Env::new();
    let mut prompt_state = PromptState::Ready;

    loop {
        match rl.readline(prompt_state.prompt()) {
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
                        Ok(Some(v)) => {
                            println!("{}", format_value(&v));
                            prompt_state.mark_ready();
                        }
                        Ok(None) => {
                            prompt_state.mark_ready();
                        }
                        Err(e) => {
                            let message = e.to_string();
                            let report = Report::new(e)
                                .with_source_code(NamedSource::new("<repl>", input.to_string()));
                            print_report(&message, input, report);
                            prompt_state.mark_error();
                        }
                    },
                    Err(e) => {
                        let message = e.to_string();
                        let report = Report::new(e)
                            .with_source_code(NamedSource::new("<repl>", input.to_string()));
                        print_report(&message, input, report);
                        prompt_state.mark_error();
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

fn colorize_title(title: &str) -> String {
    let mut colored = String::with_capacity(title.len() * (COLOR_BOLD.len() + COLOR_RESET.len()));
    let mut title_chars = title.splitn(2, ' ');
    let name = title_chars.next().unwrap_or(title);
    let rest = title_chars.next().unwrap_or("");

    for (i, ch) in name.chars().enumerate() {
        let color = TITLE_COLORS[i % TITLE_COLORS.len()];
        colored.push_str(COLOR_BOLD);
        colored.push_str(color);
        colored.push(ch);
    }
    colored.push_str(COLOR_RESET);

    if !rest.is_empty() {
        colored.push(' ');
        colored.push_str(rest);
    }

    colored
}

enum PromptState {
    Ready,
    Error,
    Warning,
}

impl PromptState {
    fn prompt(&self) -> &'static str {
        match self {
            PromptState::Ready => PROMPT_READY,
            PromptState::Error => PROMPT_ERROR,
            PromptState::Warning => PROMPT_WARNING,
        }
    }

    fn mark_ready(&mut self) {
        *self = PromptState::Ready;
    }

    fn mark_error(&mut self) {
        *self = PromptState::Error;
    }

    #[allow(dead_code)]
    fn mark_warning(&mut self) {
        *self = PromptState::Warning;
    }
}

const PROMPT_READY: &str = "\x1b[32m> \x1b[0m";
const PROMPT_ERROR: &str = "\x1b[31m> \x1b[0m";
const PROMPT_WARNING: &str = "\x1b[33m> \x1b[0m";
