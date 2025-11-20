use std::{
    fs::File,
    io::{self, BufRead, Write as IoWrite},
    path::Path,
};

use colored::Colorize;
use miette::{NamedSource, Report};

use crate::{
    interpreter::Env,
    lexer::Lexer,
    parser::Parser,
    repl::{create_editor, format_value, print_report},
    ui::colors::{
        INSTRUCTION_GREEN, PROMPT_BRACKET_ERROR, PROMPT_BRACKET_READY, PROMPT_BRACKET_WARNING,
        PROMPT_ERROR, PROMPT_READY, PROMPT_WARNING, TITLE_ACCENT_BLUE, TITLE_BRACKET_WHITE,
        TITLE_RAINBOW,
    },
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
    let color_enabled = resolve_color_setting(config.color);
    if color_enabled {
        println!("{}", colorize_title(TITLE, true));
        println!(
            "{}\n",
            "Type expressions or 'quit' to exit"
                .color(INSTRUCTION_GREEN)
                .dimmed()
        );
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

    let mut rl = create_editor(color_enabled).expect("failed to initialize REPL editor");
    let mut env = Env::new();
    let mut prompt_state = PromptState::new(color_enabled, true);

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
                if let Err(err) = rl.add_history_entry(input) {
                    eprintln!("warning: failed to record history entry: {err}");
                }

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
    let color_enabled = resolve_color_setting(config.color);
    let mut env = Env::new();
    let mut prompt_state = PromptState::new(color_enabled, true);
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
    let color_enabled = resolve_color_setting(config.color);
    let mut env = Env::new();
    let mut prompt_state = PromptState::new(color_enabled, false);
    let mut stdout = io::stdout();
    process_input(trimmed, &mut prompt_state, &mut env, &mut stdout, false)?;
    stdout.flush()
}

fn run_script_reader<R, W>(reader: R, out: &mut W, config: RunConfig) -> io::Result<()>
where
    R: BufRead,
    W: IoWrite,
{
    let color_enabled = resolve_color_setting(config.color);
    let mut env = Env::new();
    let mut prompt_state = PromptState::new(color_enabled, false);
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
    let mut colored_title = String::with_capacity(title.len() * 2);
    let mut title_parts = title.splitn(2, ' ');
    let name = title_parts.next().unwrap_or(title);
    let rest = title_parts.next().unwrap_or("");

    let mut color_index = 0;
    for ch in name.chars() {
        if ch == '[' {
            colored_title.push_str(&format!("{}", "[".color(TITLE_BRACKET_WHITE).bold()));
            continue;
        }
        let color = TITLE_RAINBOW[color_index % TITLE_RAINBOW.len()];
        colored_title.push_str(&format!("{}", ch.to_string().color(color).bold()));
        color_index += 1;
    }

    if !rest.is_empty() {
        colored_title.push(' ');
        let mut rest_chars = rest.chars();
        if let Some(first) = rest_chars.next() {
            if first == '-' {
                colored_title.push_str(&format!("{}", "-".color(TITLE_BRACKET_WHITE).bold()));
                let mut remaining: String = rest_chars.collect();
                let has_closing_bracket = remaining.ends_with(']');
                if has_closing_bracket {
                    remaining.pop();
                }
                if !remaining.is_empty() {
                    colored_title
                        .push_str(&format!("{}", remaining.color(TITLE_ACCENT_BLUE).bold()));
                }
                if has_closing_bracket {
                    colored_title.push_str(&format!("{}", "]".color(TITLE_BRACKET_WHITE).bold()));
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
                    colored_title
                        .push_str(&format!("{}", remaining.color(TITLE_ACCENT_BLUE).bold()));
                }
                if has_closing_bracket {
                    colored_title.push_str(&format!("{}", "]".color(TITLE_BRACKET_WHITE).bold()));
                }
            }
        }
    }

    colored_title
}

fn resolve_color_setting(prefer_color: bool) -> bool {
    use colored::control;
    if !prefer_color {
        return false;
    }
    #[cfg(windows)]
    {
        let _ = control::set_virtual_terminal(true);
    }
    control::set_override(true);
    true
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
        let width = Self::counter_width(self.line_number);
        let counter = format!(
            "0x{value:0width$X}",
            value = self.line_number,
            width = width
        );

        if !self.color_enabled {
            return format!("[{counter}]: ");
        }

        let (bracket_color, counter_color) = self.prompt_colors();
        let open = format!("{}", "[".color(bracket_color).bold());
        let counter_colored = format!("{}", counter.color(counter_color).bold());
        let close = format!("{}", "]".color(bracket_color).bold());
        format!("{open}{counter_colored}{close}: ")
    }

    fn prompt_colors(&self) -> (colored::Color, colored::Color) {
        match self.mode {
            PromptMode::Ready => (PROMPT_BRACKET_READY, PROMPT_READY),
            PromptMode::Error => (PROMPT_BRACKET_ERROR, PROMPT_ERROR),
            PromptMode::Warning => (PROMPT_BRACKET_WARNING, PROMPT_WARNING),
        }
    }

    fn counter_width(value: usize) -> usize {
        let mut width = 2usize;
        let max_width = (usize::BITS / 4) as usize;
        let mut threshold: usize = 0xFF;

        while value > threshold && width < max_width {
            width = width.saturating_mul(2).min(max_width);
            let bits: u32 = width
                .checked_mul(4)
                .and_then(|b| b.try_into().ok())
                .unwrap_or(usize::BITS);
            threshold = match bits {
                b if b >= usize::BITS => usize::MAX,
                b => (1usize.checked_shl(b).unwrap_or(usize::MAX)).saturating_sub(1),
            };
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ui::colors::PROMPT_WARNING;
    use colored::{Colorize, control};
    use std::{io::Cursor, sync::Once};

    fn ensure_color_override() {
        static FORCE: Once = Once::new();
        FORCE.call_once(|| control::set_override(true));
    }

    #[test]
    fn noninteractive_handles_blank_lines_and_errors() {
        let input = b"2 + 2\n\nundefined()\nquit\n";
        let reader = Cursor::new(&input[..]);
        let mut buffer = Vec::new();
        let config = RunConfig { color: false };

        run_noninteractive_with_config(reader, &mut buffer, config).expect("run noninteractive");

        let output = String::from_utf8(buffer).expect("utf8");
        assert!(
            output.contains("[0x00]: 2 + 2"),
            "missing echoed input:\n{output}"
        );
        assert!(
            output.contains("[0x00]: 4"),
            "missing value output:\n{output}"
        );
        assert!(
            output.contains("[0x01]: \n"),
            "blank line should emit empty prompt:\n{output}"
        );
        assert!(
            output.contains("[0x01]: undefined()"),
            "expected echoed failing input:\n{output}"
        );
        assert!(
            output.contains("  1 | undefined()"),
            "expected fallback diagnostic snippet:\n{output}"
        );
        assert!(
            output.contains("Goodbye!"),
            "missing goodbye message:\n{output}"
        );
    }

    #[test]
    fn counter_width_expands_in_hex_ranges() {
        assert_eq!(PromptState::counter_width(0x00), 2);
        assert_eq!(PromptState::counter_width(0xFF), 2);
        assert_eq!(PromptState::counter_width(0x100), 4);
        assert_eq!(PromptState::counter_width(0xFFFF), 4);
        assert_eq!(PromptState::counter_width(0x1_0000), 8);
    }

    #[test]
    fn counter_width_caps_at_pointer_bits() {
        let max_width = (usize::BITS / 4) as usize;
        assert_eq!(PromptState::counter_width(usize::MAX), max_width);
    }

    #[test]
    fn colorize_title_colored_and_plain() {
        ensure_color_override();
        let plain = colorize_title("[ABACUS - Calculator REPL]", false);
        assert_eq!(plain, "[ABACUS - Calculator REPL]");

        let colored = colorize_title("[ABACUS - Calculator REPL]", true);
        assert!(colored.contains('\x1b'));
        assert_ne!(colored, plain);
    }

    #[test]
    fn prompt_state_applies_warning_color() {
        ensure_color_override();
        let mut state = PromptState::new(true, true);
        state.mark_warning();
        let prompt = state.prompt();
        let expected = format!("{}", "0x00".color(PROMPT_WARNING).bold());
        assert!(
            prompt.contains(&expected),
            "prompt should contain colored counter: {prompt:?}"
        );
    }

    #[test]
    fn process_input_success_echoes_and_marks_ready() {
        let mut prompt = PromptState::new(false, true);
        prompt.mark_error();
        let mut env = Env::new();
        let mut out = Vec::new();

        process_input("1 + 1", &mut prompt, &mut env, &mut out, true).expect("process success");
        assert!(matches!(prompt.mode, PromptMode::Ready));

        let rendered = String::from_utf8(out).expect("utf8");
        assert!(rendered.contains("[0x00]: 1 + 1"));
        assert!(rendered.contains("[0x00]: 2"));
    }

    #[test]
    fn process_input_error_marks_error_without_prompt() {
        let mut prompt = PromptState::new(false, false);
        let mut env = Env::new();
        let mut out = Vec::new();

        process_input("a ==", &mut prompt, &mut env, &mut out, false)
            .expect("process failure handles");
        assert!(matches!(prompt.mode, PromptMode::Error));

        let rendered = String::from_utf8(out).expect("utf8");
        assert!(
            !rendered.contains("[0x00]:"),
            "prompt should not appear:\n{rendered}"
        );
        assert!(
            rendered.contains("  1 | a =="),
            "expected fallback diagnostic snippet:\n{rendered}"
        );
    }

    #[test]
    fn run_script_reader_executes_lines_in_single_env() {
        let script = "a = 5\na + 7\n";
        let reader = script.as_bytes();
        let mut out = Vec::new();
        run_script_reader(reader, &mut out, RunConfig { color: false }).expect("script");
        let output = String::from_utf8(out).expect("utf8");
        assert!(output.contains("12"));
    }
}
