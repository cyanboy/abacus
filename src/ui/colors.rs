use colored::Color;

pub const TITLE_RAINBOW: [Color; 6] = [
    Color::Red,
    Color::Yellow,
    Color::Green,
    Color::Cyan,
    Color::Blue,
    Color::Magenta,
];
pub const TITLE_ACCENT_BLUE: Color = Color::Blue;
pub const TITLE_BRACKET_WHITE: Color = Color::White;

pub const PROMPT_READY: Color = Color::BrightGreen;
pub const PROMPT_ERROR: Color = Color::BrightRed;
pub const PROMPT_BRACKET_READY: Color = Color::Green;
pub const PROMPT_BRACKET_ERROR: Color = Color::Red;

pub const VALUE_OUTPUT: Color = Color::BrightYellow;
pub const LITERAL_YELLOW: Color = Color::BrightYellow;
pub const FUNCTION_CYAN: Color = Color::BrightCyan;
pub const OPERATOR_BLUE: Color = Color::BrightBlue;

pub const INSTRUCTION_GREEN: Color = Color::Green;
