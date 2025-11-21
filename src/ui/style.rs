use colored::Color;

#[derive(Debug, Clone, Copy, Default)]
pub struct TextStyle {
    pub bold: bool,
    pub dim: bool,
}

pub fn colorize(text: &str, color: Color, enabled: bool) -> String {
    paint(text, color, TextStyle::default(), enabled)
}

pub fn colorize_bold(text: &str, color: Color, enabled: bool) -> String {
    paint(
        text,
        color,
        TextStyle {
            bold: true,
            dim: false,
        },
        enabled,
    )
}

pub fn colorize_dim(text: &str, color: Color, enabled: bool) -> String {
    paint(
        text,
        color,
        TextStyle {
            bold: false,
            dim: true,
        },
        enabled,
    )
}

pub fn paint(text: &str, color: Color, style: TextStyle, enabled: bool) -> String {
    if !enabled {
        return text.to_string();
    }

    let mut codes = String::new();
    if style.bold {
        codes.push_str("1;");
    }
    if style.dim {
        codes.push_str("2;");
    }
    codes.push_str(&foreground_code(color));

    format!("\x1b[{codes}m{text}\x1b[0m")
}

fn foreground_code(color: Color) -> String {
    color.to_fg_str().into_owned()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::env;

    struct EnvGuard {
        key: &'static str,
        original: Option<String>,
    }

    impl EnvGuard {
        fn set(key: &'static str, value: &str) -> Self {
            let original = env::var(key).ok();
            unsafe { env::set_var(key, value) };
            Self { key, original }
        }
    }

    impl Drop for EnvGuard {
        fn drop(&mut self) {
            match &self.original {
                Some(v) => unsafe { env::set_var(self.key, v) },
                None => unsafe { env::remove_var(self.key) },
            }
        }
    }

    #[test]
    fn colorize_bold_wraps_content() {
        let rendered = colorize_bold("hi", Color::Red, true);
        assert!(rendered.starts_with("\x1b[1;31m"));
        assert!(rendered.ends_with("\x1b[0m"));
    }

    #[test]
    fn colorize_honors_enabled_flag() {
        let rendered = colorize("hi", Color::Red, false);
        assert_eq!(rendered, "hi");
    }

    #[test]
    fn colorize_dim_applies_dim_code() {
        let rendered = colorize_dim("x", Color::Yellow, true);
        assert!(
            rendered.starts_with("\x1b[2;33m"),
            "dim yellow should use ESC[2;33m prefix, got {rendered:?}"
        );
    }

    #[test]
    fn paint_combines_bold_and_dim() {
        let rendered = paint(
            "mix",
            Color::BrightCyan,
            TextStyle {
                bold: true,
                dim: true,
            },
            true,
        );
        assert!(
            rendered.starts_with("\x1b[1;2;96m"),
            "expected bold+dim bright cyan prefix, got {rendered:?}"
        );
    }

    #[test]
    fn truecolor_renders_24bit_sequence_when_supported() {
        let _guard = EnvGuard::set("COLORTERM", "truecolor");
        let rendered = colorize("rgb", Color::TrueColor { r: 1, g: 2, b: 3 }, true);
        assert!(
            rendered.starts_with("\x1b[38;2;1;2;3m"),
            "truecolor should emit 38;2 sequence when supported, got {rendered:?}"
        );
    }
}
