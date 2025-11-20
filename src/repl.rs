use std::{
    borrow::Cow::{Borrowed, Owned},
    io::{self, Write},
};

use miette::{GraphicalReportHandler, Report};
use rustyline::{
    Editor,
    completion::Completer,
    config::{ColorMode, Configurer},
    highlight::{CmdKind, Highlighter},
    hint::Hinter,
    history::DefaultHistory,
    validate::Validator,
};
use unicode_width::UnicodeWidthStr;

use crate::{
    interpreter::Value,
    lexer::{
        Lexer,
        token::{Token, TokenKind},
    },
    ui::colors::{FUNCTION_CYAN, LITERAL_YELLOW, OPERATOR_BLUE, RESET, VALUE_OUTPUT},
};

#[derive(Clone, Copy)]
pub struct ReplHelper {
    color_enabled: bool,
}

impl ReplHelper {
    pub fn new(color_enabled: bool) -> Self {
        Self { color_enabled }
    }
}

impl rustyline::Helper for ReplHelper {}

impl Completer for ReplHelper {
    type Candidate = String;
}

impl Hinter for ReplHelper {
    type Hint = String;
}

impl Validator for ReplHelper {}

impl Highlighter for ReplHelper {
    fn highlight<'l>(&self, line: &'l str, _pos: usize) -> std::borrow::Cow<'l, str> {
        if !self.color_enabled {
            return Borrowed(line);
        }
        let lexer = Lexer::new(line);
        let mut tokens = Vec::new();

        for token in lexer {
            match token {
                Ok(token) => tokens.push(token),
                Err(_) => return Borrowed(line),
            }
        }

        if tokens.is_empty() {
            return Borrowed(line);
        }

        let mut highlighted = String::with_capacity(line.len());
        let mut cursor = 0;
        let mut colored = false;

        for (idx, token) in tokens.iter().enumerate() {
            let span = token.span;
            if span.start > cursor {
                highlighted.push_str(&line[cursor..span.start]);
            }

            let segment = &line[span.start..span.end];
            if let Some(color) = highlight_color(&tokens, idx, self.color_enabled) {
                highlighted.push_str(color);
                highlighted.push_str(segment);
                highlighted.push_str(RESET);
                colored = true;
            } else {
                highlighted.push_str(segment);
            }

            cursor = span.end;
        }

        if cursor < line.len() {
            highlighted.push_str(&line[cursor..]);
        }

        if colored {
            Owned(highlighted)
        } else {
            Borrowed(line)
        }
    }

    fn highlight_char(&self, _line: &str, _pos: usize, _kind: CmdKind) -> bool {
        true
    }
}

pub type ReplEditor = Editor<ReplHelper, DefaultHistory>;

pub fn create_editor(color_enabled: bool) -> rustyline::Result<ReplEditor> {
    let mut rl = Editor::<ReplHelper, DefaultHistory>::new()?;
    rl.set_helper(Some(ReplHelper::new(color_enabled)));
    rl.set_color_mode(if color_enabled {
        ColorMode::Forced
    } else {
        ColorMode::Disabled
    });
    Ok(rl)
}

pub fn format_value(value: &Value, color_enabled: bool) -> String {
    if !color_enabled {
        return value.to_string();
    }
    let style = match value {
        Value::Int(_) | Value::Float(_) | Value::Bool(_) => VALUE_OUTPUT,
    };
    format!("{style}{value}{RESET}")
}

pub fn print_report<W: Write>(
    writer: &mut W,
    source: &str,
    report: Report,
    color_enabled: bool,
) -> io::Result<()> {
    if !color_enabled {
        return render_fallback(writer, source, &report);
    }
    let has_labels = report
        .labels()
        .map(|labels| labels.count() > 0)
        .unwrap_or(false);

    let handler = GraphicalReportHandler::new().without_cause_chain();
    let mut rendered = String::new();
    if handler
        .render_report(&mut rendered, report.as_ref())
        .is_err()
    {
        render_fallback(writer, source, &report)?;
        return Ok(());
    }

    if rendered.trim().is_empty() || !has_labels {
        render_fallback(writer, source, &report)?;
        return Ok(());
    }

    write!(writer, "{rendered}")?;
    if !rendered.ends_with('\n') {
        writeln!(writer)?;
    }
    Ok(())
}

fn render_fallback<W: Write>(writer: &mut W, source: &str, report: &Report) -> io::Result<()> {
    if source.is_empty() {
        return Ok(());
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
        .unwrap_or(source.len());
    let byte_index = label_offset.min(source.len());
    let prefix = &source[..byte_index];
    let caret_pad = " ".repeat(UnicodeWidthStr::width(prefix));

    writeln!(writer)?;
    writeln!(writer, "  1 | {source}")?;
    writeln!(writer, "    | {caret_pad}^")?;
    Ok(())
}

fn highlight_color<'a>(
    tokens: &[Token<'a>],
    index: usize,
    color_enabled: bool,
) -> Option<&'static str> {
    if !color_enabled {
        return None;
    }
    match tokens[index].kind {
        TokenKind::Integer(_) | TokenKind::Float(_) | TokenKind::Bool(_) => Some(LITERAL_YELLOW),
        TokenKind::Identifier(_) if is_function_name(tokens, index) => Some(FUNCTION_CYAN),
        TokenKind::Assign
        | TokenKind::Plus
        | TokenKind::Minus
        | TokenKind::Star
        | TokenKind::Slash
        | TokenKind::Percent
        | TokenKind::Bang
        | TokenKind::Caret
        | TokenKind::Eq
        | TokenKind::Gt
        | TokenKind::GtEq
        | TokenKind::Lt
        | TokenKind::LtEq
        | TokenKind::Ne
        | TokenKind::BitOr
        | TokenKind::Or
        | TokenKind::BitAnd
        | TokenKind::And
        | TokenKind::OpenParen
        | TokenKind::CloseParen => Some(OPERATOR_BLUE),
        _ => None,
    }
}

fn is_function_name<'a>(tokens: &[Token<'a>], index: usize) -> bool {
    matches!(
        tokens.get(index + 1).map(|next| &next.kind),
        Some(TokenKind::OpenParen)
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::borrow::Cow;

    #[test]
    fn highlight_without_color_passes_through() {
        let helper = ReplHelper::new(false);
        match helper.highlight("1 + 2", 0) {
            Cow::Borrowed(text) => assert_eq!(text, "1 + 2"),
            Cow::Owned(_) => panic!("should not allocate when color disabled"),
        }
    }

    #[test]
    fn highlight_with_color_marks_tokens() {
        let helper = ReplHelper::new(true);
        let highlighted = helper.highlight("foo(1 + 2)", 0).into_owned();
        assert!(highlighted.contains(FUNCTION_CYAN));
        assert!(highlighted.contains(OPERATOR_BLUE));
        assert!(highlighted.contains(RESET));
    }

    #[test]
    fn format_value_adds_style_when_colored() {
        let value = Value::Int(8);
        assert_eq!(format_value(&value, false), "8");
        let colored = format_value(&value, true);
        assert!(colored.starts_with(VALUE_OUTPUT));
        assert!(colored.ends_with(RESET));
    }

    #[test]
    fn print_report_without_color_uses_fallback() {
        let mut out = Vec::new();
        let report = Report::msg("boom");
        print_report(&mut out, "a + b", report, false).expect("print_report");
        let rendered = String::from_utf8(out).expect("utf8");
        assert!(rendered.contains("a + b"));
        assert!(rendered.contains("^"));
    }
}
