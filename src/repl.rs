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
    ui::colors::{FUNCTION_CYAN, LITERAL_YELLOW, OPERATOR_BLUE, VALUE_OUTPUT},
    ui::style::colorize,
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
            match highlight_color(&tokens, idx) {
                Some(color) => {
                    highlighted.push_str(&colorize(segment, color, self.color_enabled));
                    colored = true;
                }
                None => highlighted.push_str(segment),
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
        ColorMode::Enabled
    } else {
        ColorMode::Disabled
    });
    Ok(rl)
}

pub fn format_value(value: &Value, color_enabled: bool) -> String {
    colorize(&value.to_string(), VALUE_OUTPUT, color_enabled)
}

pub fn print_report<W: Write>(
    writer: &mut W,
    source: &str,
    report: Report,
    color_enabled: bool,
) -> io::Result<()> {
    let has_labels = report
        .labels()
        .map(|labels| labels.count() > 0)
        .unwrap_or(false);

    if !has_labels {
        writeln!(writer, "{report}")?;
        return Ok(());
    }

    if !color_enabled {
        let mut rendered = String::new();
        let handler = GraphicalReportHandler::new().without_cause_chain();
        if handler
            .render_report(&mut rendered, report.as_ref())
            .is_ok()
            && !rendered.trim().is_empty()
        {
            write!(writer, "{rendered}")?;
            if !rendered.ends_with('\n') {
                writeln!(writer)?;
            }
            return Ok(());
        }
        writeln!(writer, "{report}")?;
        render_fallback(writer, source, &report)?;
        return Ok(());
    }

    let handler = GraphicalReportHandler::new().without_cause_chain();
    let mut rendered = String::new();
    if handler
        .render_report(&mut rendered, report.as_ref())
        .is_err()
        || rendered.trim().is_empty()
    {
        writeln!(writer, "{report}")?;
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

fn highlight_color<'a>(tokens: &[Token<'a>], index: usize) -> Option<colored::Color> {
    match tokens[index].kind {
        TokenKind::Integer { .. } | TokenKind::Float(_) | TokenKind::Bool(_) => {
            Some(LITERAL_YELLOW)
        }
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
    use crate::ui::style::colorize;
    use rustyline::highlight::CmdKind;
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
        let expected_fn = colorize("foo", FUNCTION_CYAN, true);
        assert!(
            highlighted.contains(&expected_fn),
            "function name should be highlighted: {highlighted:?}"
        );
        let expected_op = colorize("+", OPERATOR_BLUE, true);
        assert!(
            highlighted.contains(&expected_op),
            "operator should be highlighted: {highlighted:?}"
        );
    }

    #[test]
    fn format_value_adds_style_when_colored() {
        let value = Value::Int(8);
        assert_eq!(format_value(&value, false), "8");
        let colored_value = format_value(&value, true);
        let expected = colorize("8", VALUE_OUTPUT, true);
        assert_eq!(colored_value, expected);
    }

    #[test]
    fn create_editor_sets_helper_color_flag() {
        let colorful = create_editor(true).expect("colorful editor");
        assert!(colorful.helper().expect("helper installed").color_enabled);

        let plain = create_editor(false).expect("plain editor");
        assert!(!plain.helper().expect("helper installed").color_enabled);
    }

    #[test]
    fn highlight_char_always_true() {
        let helper = ReplHelper::new(true);
        assert!(helper.highlight_char("", 0, CmdKind::Other));
    }

    #[test]
    fn print_report_with_color_and_no_labels_falls_back() {
        let mut out = Vec::new();
        let report = Report::msg("boom");
        print_report(&mut out, "a + b", report, true).expect("print_report");
        let rendered = String::from_utf8(out).expect("utf8");
        assert!(
            rendered.contains("boom"),
            "message should be printed: {rendered:?}"
        );
    }

    #[test]
    fn render_fallback_no_source_is_noop() {
        let mut out = Vec::new();
        let report = Report::msg("boom");
        render_fallback(&mut out, "", &report).expect("fallback empty");
        assert!(out.is_empty());
    }

    #[test]
    fn print_report_without_color_uses_fallback() {
        let mut out = Vec::new();
        let report = Report::msg("boom");
        print_report(&mut out, "a + b", report, false).expect("print_report");
        let rendered = String::from_utf8(out).expect("utf8");
        assert!(
            rendered.contains("boom"),
            "message should be printed: {rendered:?}"
        );
    }
}
