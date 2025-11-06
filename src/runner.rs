use std::{
    fmt::Write,
    fs::File,
    io::{self, BufRead, Write as IoWrite},
    path::Path,
};

use miette::{NamedSource, Report};

use crate::{
    colors::{
        BOLD, INSTRUCTION_DIM_GREEN, PROMPT_BRACKET_ERROR, PROMPT_BRACKET_READY,
        PROMPT_BRACKET_WARNING, PROMPT_ERROR, PROMPT_READY, PROMPT_WARNING, RESET,
        TITLE_ACCENT_BLUE, TITLE_BRACKET_WHITE, TITLE_RAINBOW,
    },
    interpreter::Env,
    lexer::Lexer,
    parser::Parser,
    repl::{create_editor, format_value, print_report},
};

const TITLE: &str = "[ABACUS - Calculator REPL]";

#[derive(Debug, Clone, Copy)]
pub struct RunConfig {
    pub color: bool,
}

impl Default for RunConfig {
    fn default() -> Self {
        Self { color: true }
    }
}

pub fn run() {
    run_with_config(RunConfig::default());
}

pub fn run_with_config(config: RunConfig) {
    if config.color {
        println!("{}", colorize_title(TITLE, true));
        println!("{INSTRUCTION_DIM_GREEN}Type expressions or 'quit' to exit{RESET}\n");
    } else {
        println!("{}", colorize_title(TITLE, false));
        println!("Type expressions or 'quit' to exit\n");
    }

    if std::env::var_os("ABACUS_TEST_MODE").is_some() {
        let stdin = io::stdin();
        let mut stdout = io::stdout();
        run_noninteractive_with_config(stdin.lock(), &mut stdout, config)
            .expect("failed to run non-interactive REPL");
        return;
    }

    let mut rl = create_editor(config.color).expect("failed to initialize REPL editor");
    let mut env = Env::new();
    let mut prompt_state = PromptState::new(config.color, true);

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

pub fn run_noninteractive<R, W>(reader: R, out: &mut W) -> io::Result<()>
where
    R: BufRead,
    W: IoWrite,
{
    run_noninteractive_with_config(reader, out, RunConfig::default())
}

pub fn run_noninteractive_with_config<R, W>(
    mut reader: R,
    out: &mut W,
    config: RunConfig,
) -> io::Result<()>
where
    R: BufRead,
    W: IoWrite,
{
    let mut env = Env::new();
    let mut prompt_state = PromptState::new(config.color, true);
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

pub fn run_file<P: AsRef<Path>>(path: P, config: RunConfig) -> io::Result<()> {
    let file = File::open(path)?;
    let reader = io::BufReader::new(file);
    let mut stdout = io::stdout();
    run_script_reader(reader, &mut stdout, config)
}

pub fn run_expression(expr: &str, config: RunConfig) -> io::Result<()> {
    let trimmed = expr.trim();
    if trimmed.is_empty() {
        return Ok(());
    }
    let mut env = Env::new();
    let mut prompt_state = PromptState::new(config.color, false);
    let mut stdout = io::stdout();
    process_input(trimmed, &mut prompt_state, &mut env, &mut stdout, false)?;
    stdout.flush()
}

fn run_script_reader<R, W>(reader: R, out: &mut W, config: RunConfig) -> io::Result<()>
where
    R: BufRead,
    W: IoWrite,
{
    let mut env = Env::new();
    let mut prompt_state = PromptState::new(config.color, false);
    for line in reader.lines() {
        let line = line?;
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }
        process_input(trimmed, &mut prompt_state, &mut env, out, false)?;
        prompt_state.advance();
    }
    out.flush()
}

fn colorize_title(title: &str, color_enabled: bool) -> String {
    if !color_enabled {
        return title.to_string();
    }
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
                let value = format_value(&v, prompt_state.color_enabled());
                if prompt_state.show_prompt() {
                    writeln!(out, "{}", prompt_state.format_with_prompt(&value))?;
                    writeln!(out)?;
                } else {
                    writeln!(out, "{value}")?;
                }
            }
            Ok(None) => {
                prompt_state.mark_ready();
                if prompt_state.show_prompt() {
                    writeln!(out)?;
                }
            }
            Err(e) => {
                prompt_state.mark_error();
                let report =
                    Report::new(e).with_source_code(NamedSource::new("<repl>", input.to_string()));
                print_report(out, input, report, prompt_state.color_enabled())?;
                if prompt_state.show_prompt() {
                    writeln!(out)?;
                }
            }
        },
        Err(e) => {
            prompt_state.mark_error();
            let report =
                Report::new(e).with_source_code(NamedSource::new("<repl>", input.to_string()));
            print_report(out, input, report, prompt_state.color_enabled())?;
            if prompt_state.show_prompt() {
                writeln!(out)?;
            }
        }
    }

    out.flush()?;
    Ok(())
}

struct PromptState {
    mode: PromptMode,
    line_number: usize,
    color_enabled: bool,
    show_prompt: bool,
}

impl PromptState {
    fn new(color_enabled: bool, show_prompt: bool) -> Self {
        Self {
            mode: PromptMode::Ready,
            line_number: 0,
            color_enabled,
            show_prompt,
        }
    }

    fn prompt(&self) -> String {
        if self.show_prompt {
            self.prompt_prefix()
        } else {
            String::new()
        }
    }

    fn format_with_prompt(&self, content: &str) -> String {
        if self.show_prompt {
            let mut line = self.prompt_prefix();
            line.push_str(content);
            line
        } else {
            content.to_string()
        }
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
        if self.color_enabled {
            prompt.push_str(RESET);
            prompt.push_str(bracket_color);
        }
        prompt.push('[');
        if self.color_enabled {
            prompt.push_str(RESET);
            prompt.push_str(counter_color);
        }
        prompt.push_str("0x");
        write!(
            &mut prompt,
            "{value:0width$X}",
            value = self.line_number,
            width = width
        )
        .unwrap();
        if self.color_enabled {
            prompt.push_str(RESET);
            prompt.push_str(bracket_color);
        }
        prompt.push_str("]:");
        if self.color_enabled {
            prompt.push_str(RESET);
        }
        prompt.push(' ');

        prompt
    }

    fn prompt_colors(&self) -> (&'static str, &'static str) {
        if !self.color_enabled {
            ("", "")
        } else {
            match self.mode {
                PromptMode::Ready => (PROMPT_BRACKET_READY, PROMPT_READY),
                PromptMode::Error => (PROMPT_BRACKET_ERROR, PROMPT_ERROR),
                PromptMode::Warning => (PROMPT_BRACKET_WARNING, PROMPT_WARNING),
            }
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

    fn color_enabled(&self) -> bool {
        self.color_enabled
    }

    fn show_prompt(&self) -> bool {
        self.show_prompt
    }
}

enum PromptMode {
    Ready,
    Error,
    Warning,
}
