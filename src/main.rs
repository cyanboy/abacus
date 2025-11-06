mod colors;
mod eval;
mod lexer;
mod parser;
mod repl;

use miette::{NamedSource, Report};

use crate::{
    colors::{
        BOLD, INSTRUCTION_DIM_GREEN, PROMPT_BRACKET_ERROR, PROMPT_BRACKET_READY,
        PROMPT_BRACKET_WARNING, PROMPT_ERROR, PROMPT_READY, PROMPT_WARNING, RESET,
        TITLE_ACCENT_BLUE, TITLE_BRACKET_WHITE, TITLE_RAINBOW,
    },
    eval::Env,
    lexer::Lexer,
    parser::Parser,
    repl::{create_editor, format_value, print_report},
};

use std::{
    fmt::Write,
    io::{self, BufRead, Write as IoWrite},
};

const TITLE: &str = "[ABACUS - Calculator REPL]";

fn main() {
    println!("{}", colorize_title(TITLE));
    println!("{INSTRUCTION_DIM_GREEN}Type expressions or 'quit' to exit{RESET}\n");

    if std::env::var_os("ABACUS_TEST_MODE").is_some() {
        let stdin = io::stdin();
        let mut stdout = io::stdout();
        run_noninteractive(stdin.lock(), &mut stdout).expect("failed to run non-interactive REPL");
        return;
    }

    let mut rl = create_editor().expect("failed to initialize REPL editor");
    let mut env = Env::new();
    let mut prompt_state = PromptState::new();

    loop {
        let prompt = prompt_state.prompt();
        match rl.readline(&prompt) {
            Ok(line) => {
                let input = line.trim();
                if input.is_empty() {
                    println!();
                    let _ = io::stdout().flush();
                    continue;
                }
                rl.add_history_entry(input).unwrap();

                if input == "quit" || input == "exit" {
                    println!("Goodbye!");
                    break;
                }

                let mut stdout = io::stdout();
                if let Err(err) =
                    process_input(input, &mut prompt_state, &mut env, &mut stdout, false)
                {
                    eprintln!("I/O error while processing input: {err}");
                }
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
        self.prompt_prefix()
    }

    fn format_with_prompt(&self, content: &str) -> String {
        let mut line = self.prompt_prefix();
        line.push_str(content);
        line
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

    fn prompt_prefix(&self) -> String {
        let (bracket_color, counter_color) = self.prompt_colors();
        let width = Self::counter_width(self.line_number);

        let mut prompt = String::new();
        prompt.push_str(RESET);
        prompt.push_str(bracket_color);
        prompt.push('[');
        prompt.push_str(RESET);
        prompt.push_str(counter_color);
        prompt.push_str("0x");
        write!(
            &mut prompt,
            "{value:0width$X}",
            value = self.line_number,
            width = width
        )
        .unwrap();
        prompt.push_str(RESET);
        prompt.push_str(bracket_color);
        prompt.push_str("]:");
        prompt.push_str(RESET);
        prompt.push(' ');

        prompt
    }

    fn prompt_colors(&self) -> (&'static str, &'static str) {
        match self.mode {
            PromptMode::Ready => (PROMPT_BRACKET_READY, PROMPT_READY),
            PromptMode::Error => (PROMPT_BRACKET_ERROR, PROMPT_ERROR),
            PromptMode::Warning => (PROMPT_BRACKET_WARNING, PROMPT_WARNING),
        }
    }

    fn counter_width(value: usize) -> usize {
        let mut width = 2;
        let mut threshold: usize = 0xFF;
        while value > threshold {
            width *= 2;
            threshold = (1usize << (width * 4)) - 1;
            if width >= 32 {
                break;
            }
        }
        width
    }
}

enum PromptMode {
    Ready,
    Error,
    Warning,
}

fn run_noninteractive<R, W>(mut reader: R, out: &mut W) -> io::Result<()>
where
    R: BufRead,
    W: IoWrite,
{
    let mut env = Env::new();
    let mut prompt_state = PromptState::new();
    let mut line = String::new();

    loop {
        line.clear();
        let bytes = reader.read_line(&mut line)?;
        if bytes == 0 {
            writeln!(out, "Goodbye!")?;
            break;
        }

        let trimmed = line.trim();
        if trimmed.is_empty() {
            writeln!(out)?;
            writeln!(out, "{}", prompt_state.prompt())?;
            out.flush()?;
            continue;
        }

        if trimmed == "quit" || trimmed == "exit" {
            writeln!(out, "Goodbye!")?;
            break;
        }

        process_input(trimmed, &mut prompt_state, &mut env, out, true)?;

        prompt_state.advance();
        writeln!(out, "{}", prompt_state.prompt())?;
        out.flush()?;
    }

    out.flush()?;
    Ok(())
}

fn process_input<W: IoWrite>(
    input: &str,
    prompt_state: &mut PromptState,
    env: &mut Env,
    out: &mut W,
    echo_input: bool,
) -> io::Result<()> {
    if echo_input {
        writeln!(out, "{}", prompt_state.format_with_prompt(input))?;
    }

    let mut parser = Parser::new(Lexer::new(input));
    match parser.parse() {
        Ok(stmt) => match env.eval_stmt(&stmt) {
            Ok(Some(v)) => {
                prompt_state.mark_ready();
                let value = format_value(&v);
                writeln!(out, "{}", prompt_state.format_with_prompt(&value))?;
                writeln!(out)?;
            }
            Ok(None) => {
                prompt_state.mark_ready();
                writeln!(out)?;
            }
            Err(e) => {
                prompt_state.mark_error();
                let report =
                    Report::new(e).with_source_code(NamedSource::new("<repl>", input.to_string()));
                print_report(out, "", input, report)?;
                writeln!(out)?;
            }
        },
        Err(e) => {
            prompt_state.mark_error();
            let report =
                Report::new(e).with_source_code(NamedSource::new("<repl>", input.to_string()));
            print_report(out, "", input, report)?;
            writeln!(out)?;
        }
    }

    out.flush()?;
    Ok(())
}
