use miette::{NamedSource, Report};

mod colors;
mod eval;
mod lexer;
mod parser;
mod repl;

use crate::{
    colors::{
        BOLD, INSTRUCTION_DIM_GREEN, PROMPT_COUNTER, PROMPT_ERROR, PROMPT_PARENS, PROMPT_READY,
        PROMPT_WARNING, RESET, TITLE_ACCENT_BLUE, TITLE_BRACKET_WHITE, TITLE_RAINBOW,
    },
    eval::Env,
    lexer::Lexer,
    parser::Parser,
    repl::{create_editor, format_value, print_report},
};

use std::fmt::Write;
use std::io::{self, Write as IoWrite};

const TITLE: &str = "[ABACUS - Calculator REPL]";

fn main() {
    println!("{}", colorize_title(TITLE));
    println!("{INSTRUCTION_DIM_GREEN}Type expressions or 'quit' to exit{RESET}\n");

    let mut rl = create_editor().expect("failed to initialize REPL editor");
    let mut env = Env::new();
    let mut prompt_state = PromptState::new();

    loop {
        let prompt = prompt_state.prompt();
        match rl.readline(&prompt) {
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
                            let _ = io::stdout().flush();
                            prompt_state.mark_ready();
                        }
                        Ok(None) => {
                            prompt_state.mark_ready();
                        }
                        Err(e) => {
                            println!();
                            let message = e.to_string();
                            let report = Report::new(e)
                                .with_source_code(NamedSource::new("<repl>", input.to_string()));
                            print_report(&message, input, report);
                            println!();
                            let _ = io::stdout().flush();
                            let _ = io::stderr().flush();
                            prompt_state.mark_error();
                        }
                    },
                    Err(e) => {
                        println!();
                        let message = e.to_string();
                        let report = Report::new(e)
                            .with_source_code(NamedSource::new("<repl>", input.to_string()));
                        print_report(&message, input, report);
                        println!();
                        let _ = io::stdout().flush();
                        let _ = io::stderr().flush();
                        prompt_state.mark_error();
                    }
                };

                prompt_state.advance();
            }
            Err(_) => {
                println!("Goodbye!");
                break;
            }
        }
    }
}

fn colorize_title(title: &str) -> String {
    let mut colored = String::with_capacity(title.len() * (BOLD.len() + RESET.len()));
    let mut title_chars = title.splitn(2, ' ');
    let name = title_chars.next().unwrap_or(title);
    let rest = title_chars.next().unwrap_or("");

    let mut color_index = 0;
    for ch in name.chars() {
        if ch == '[' {
            colored.push_str(BOLD);
            colored.push_str(TITLE_BRACKET_WHITE);
            colored.push(ch);
            colored.push_str(RESET);
            continue;
        }
        let color = TITLE_RAINBOW[color_index % TITLE_RAINBOW.len()];
        colored.push_str(BOLD);
        colored.push_str(color);
        colored.push(ch);
        color_index += 1;
    }
    colored.push_str(RESET);

    if !rest.is_empty() {
        colored.push(' ');
        let mut rest_chars = rest.chars();
        if let Some(first) = rest_chars.next() {
            if first == '-' {
                colored.push_str(BOLD);
                colored.push_str(TITLE_BRACKET_WHITE);
                colored.push(first);
                colored.push_str(RESET);

                let mut remaining: String = rest_chars.collect();
                let has_closing_bracket = remaining.ends_with(']');
                if has_closing_bracket {
                    remaining.pop();
                }

                if !remaining.is_empty() {
                    colored.push_str(BOLD);
                    colored.push_str(TITLE_ACCENT_BLUE);
                    colored.push_str(&remaining);
                    colored.push_str(RESET);
                }

                if has_closing_bracket {
                    colored.push_str(BOLD);
                    colored.push_str(TITLE_BRACKET_WHITE);
                    colored.push(']');
                    colored.push_str(RESET);
                }
            } else {
                let mut remaining = String::new();
                remaining.push(first);
                remaining.extend(rest_chars);

                let has_closing_bracket = remaining.ends_with(']');
                if has_closing_bracket {
                    remaining.pop();
                }

                if !remaining.is_empty() {
                    colored.push_str(BOLD);
                    colored.push_str(TITLE_ACCENT_BLUE);
                    colored.push_str(&remaining);
                    colored.push_str(RESET);
                }

                if has_closing_bracket {
                    colored.push_str(BOLD);
                    colored.push_str(TITLE_BRACKET_WHITE);
                    colored.push(']');
                    colored.push_str(RESET);
                }
            }
        }
    }

    colored
}

struct PromptState {
    mode: PromptMode,
    line_number: usize,
}

impl PromptState {
    fn new() -> Self {
        Self {
            mode: PromptMode::Ready,
            line_number: 0,
        }
    }

    fn prompt(&self) -> String {
        let arrow_color = match self.mode {
            PromptMode::Ready => PROMPT_READY,
            PromptMode::Error => PROMPT_ERROR,
            PromptMode::Warning => PROMPT_WARNING,
        };

        let mut prompt = String::new();
        prompt.push_str(RESET);
        prompt.push_str(PROMPT_PARENS);
        prompt.push('(');
        prompt.push_str(PROMPT_COUNTER);
        prompt.push_str("0x");
        let mut width = 2;
        let mut threshold: usize = 0xFF;
        while self.line_number > threshold {
            width *= 2;
            threshold = (1usize << (width * 4)) - 1;
            if width >= 32 {
                break;
            }
        }
        write!(
            &mut prompt,
            "{value:0width$X}",
            value = self.line_number,
            width = width
        )
        .unwrap();
        prompt.push_str(RESET);
        prompt.push_str(PROMPT_PARENS);
        prompt.push(')');
        prompt.push(' ');
        prompt.push_str(arrow_color);
        prompt.push('=');
        prompt.push('>');
        prompt.push(' ');
        prompt.push_str(RESET);

        prompt
    }

    fn mark_ready(&mut self) {
        self.mode = PromptMode::Ready;
    }

    fn mark_error(&mut self) {
        self.mode = PromptMode::Error;
    }

    #[allow(dead_code)]
    fn mark_warning(&mut self) {
        self.mode = PromptMode::Warning;
    }

    fn advance(&mut self) {
        self.line_number += 1;
    }
}

enum PromptMode {
    Ready,
    Error,
    Warning,
}
