use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(v) => write!(f, "{v}"),
            Value::Float(v) => write!(f, "{v}"),
            Value::Bool(v) => write!(f, "{v}"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Value;

    #[test]
    fn display_formats_variants_plainly() {
        assert_eq!(Value::Int(-7).to_string(), "-7");
        assert_eq!(Value::Float(3.25).to_string(), "3.25");
        assert_eq!(Value::Bool(false).to_string(), "false");
    }
}
