pub const RESET: &str = "\x1b[0m";
pub const BOLD: &str = "\x1b[1m";

// Title palette
pub const TITLE_RAINBOW: [&str; 6] = [
    "\x1b[31m", "\x1b[33m", "\x1b[32m", "\x1b[36m", "\x1b[34m", "\x1b[35m",
];
pub const TITLE_ACCENT_BLUE: &str = "\x1b[34m";
pub const TITLE_BRACKET_WHITE: &str = "\x1b[37m";

// Instruction line
pub const INSTRUCTION_DIM_GREEN: &str = "\x1b[2;32m";

// Prompt styling
pub const PROMPT_READY: &str = "\x1b[1;92m";
pub const PROMPT_ERROR: &str = "\x1b[1;31m";
pub const PROMPT_WARNING: &str = "\x1b[1;33m";
pub const PROMPT_COUNTER: &str = "\x1b[2;37m";
pub const PROMPT_PARENS: &str = "\x1b[1;35m";

// REPL syntax highlighting
pub const LITERAL_YELLOW: &str = "\x1b[93m";
pub const FUNCTION_CYAN: &str = "\x1b[96m";
pub const OPERATOR_BLUE: &str = "\x1b[94m";

// Output styling
pub const VALUE_OUTPUT: &str = "\x1b[93m";
