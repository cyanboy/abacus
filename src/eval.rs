use std::{collections::HashMap, fmt, rc::Rc};
use thiserror::Error;

use miette::{Diagnostic, SourceSpan};

use crate::{lexer::token::Span, parser::ast::*};

#[derive(Debug, Error, Diagnostic)]
pub enum EvalError {
    #[error("undefined variable: {name}")]
    UndefinedVar {
        name: String,
        #[label("not defined here")]
        span: Option<SourceSpan>,
    },

    #[error("undefined function: {name}")]
    UndefinedFunc {
        name: String,
        #[label("not defined here")]
        span: Option<SourceSpan>,
    },

    #[error("no matching arm for function: {name}")]
    NoMatchingArm {
        name: String,
        #[label("no matching arm")]
        span: Option<SourceSpan>,
    },

    #[error("type error: {message}")]
    TypeError {
        message: &'static str,
        #[label("type error")]
        span: Option<SourceSpan>,
    },

    #[error("division by zero")]
    DivideByZero {
        #[label("division by zero")]
        span: Option<SourceSpan>,
    },
}

impl EvalError {
    fn undefined_var(name: String, span: Span) -> Self {
        Self::UndefinedVar {
            name,
            span: Some(span.into_source_span()),
        }
    }

    fn undefined_func(name: String, span: Span) -> Self {
        Self::UndefinedFunc {
            name,
            span: Some(span.into_source_span()),
        }
    }

    fn no_matching_arm(name: String, span: Span) -> Self {
        Self::NoMatchingArm {
            name,
            span: Some(span.into_source_span()),
        }
    }

    fn type_error(message: &'static str, span: Span) -> Self {
        Self::TypeError {
            message,
            span: Some(span.into_source_span()),
        }
    }

    fn divide_by_zero(span: Span) -> Self {
        Self::DivideByZero {
            span: Some(span.into_source_span()),
        }
    }

    fn with_span(self, span: Span) -> Self {
        let span = Some(span.into_source_span());
        match self {
            EvalError::UndefinedVar { name, .. } => EvalError::UndefinedVar { name, span },
            EvalError::UndefinedFunc { name, .. } => EvalError::UndefinedFunc { name, span },
            EvalError::NoMatchingArm { name, .. } => EvalError::NoMatchingArm { name, span },
            EvalError::TypeError { message, .. } => EvalError::TypeError { message, span },
            EvalError::DivideByZero { .. } => EvalError::DivideByZero { span },
        }
    }
}

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

#[derive(Debug)]
pub struct Env {
    // scope stack: global frame at index 0; top is current frame
    scopes: Vec<HashMap<String, Value>>,
    // function name -> list of arms (pattern + body) ordered by specificity
    funcs: HashMap<String, Vec<Rc<FuncArm>>>,
}

impl Default for Env {
    fn default() -> Self {
        Self::new()
    }
}

impl Env {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
            funcs: HashMap::new(),
        }
    }

    fn get_var(&self, name: &str) -> Option<Value> {
        for frame in self.scopes.iter().rev() {
            if let Some(v) = frame.get(name) {
                return Some(v.clone());
            }
        }
        None
    }
    fn set_var(&mut self, name: String, v: Value) {
        self.scopes.last_mut().unwrap().insert(name, v);
    }
    fn push_frame(&mut self, bindings: HashMap<String, Value>) {
        self.scopes.push(bindings);
    }
    fn pop_frame(&mut self) {
        self.scopes.pop();
    }

    pub fn eval_stmt(&mut self, s: &Stmt) -> Result<Option<Value>, EvalError> {
        match s {
            Stmt::Assignment { name, value } => {
                let v = self.eval_expr(value)?;
                self.set_var(name.clone(), v);
                Ok(None)
            }
            Stmt::FunctionDefinition { name, arms } => {
                let entry = self.funcs.entry(name.clone()).or_default();
                entry.extend(arms.iter().cloned().map(Rc::new));
                entry.sort_by(|a, b| {
                    pattern_specificity(&b.params).cmp(&pattern_specificity(&a.params))
                });
                Ok(None)
            }
            Stmt::Expression(e) => self.eval_expr(e).map(Some),
        }
    }

    fn eval_expr(&mut self, e: &Expr) -> Result<Value, EvalError> {
        match e {
            Expr::Lit(Literal::Int(n), _) => Ok(Value::Int(*n)),
            Expr::Lit(Literal::Float(x), _) => Ok(Value::Float(*x)),
            Expr::Lit(Literal::Bool(b), _) => Ok(Value::Bool(*b)),

            Expr::Identifier(id, span) => self
                .get_var(id)
                .ok_or_else(|| EvalError::undefined_var(id.clone(), *span)),

            Expr::Group(inner, _) => self.eval_expr(inner),

            Expr::Unary { op, span, rhs } => {
                let v = self.eval_expr(rhs)?;
                match (op, v) {
                    (UnaryOp::Neg, Value::Int(i)) => Ok(Value::Int(-i)),
                    (UnaryOp::Neg, Value::Float(f)) => Ok(Value::Float(-f)),
                    (UnaryOp::Not, Value::Bool(b)) => Ok(Value::Bool(!b)),
                    _ => Err(EvalError::type_error("invalid unary operand", *span)),
                }
            }

            Expr::Binary { lhs, op, span, rhs } => self.eval_bin(*span, op, lhs, rhs),

            Expr::Call { callee, args, span } => {
                let (fname, fname_span) = match &**callee {
                    Expr::Identifier(name, span) => (name.clone(), *span),
                    other => {
                        return Err(EvalError::type_error(
                            "callee must be identifier",
                            other.span(),
                        ));
                    }
                };

                let arms: Vec<Rc<FuncArm>> = self
                    .funcs
                    .get(&fname)
                    .cloned()
                    .ok_or_else(|| EvalError::undefined_func(fname.clone(), fname_span))?;

                let argv: Vec<Value> = args
                    .iter()
                    .map(|a| self.eval_expr(a))
                    .collect::<Result<_, _>>()?;

                for arm in &arms {
                    let arm = arm.as_ref();
                    if arm.params.len() != argv.len() {
                        continue;
                    }
                    if let Some(bindings) = match_and_bind(&arm.params, &argv, *span)? {
                        self.push_frame(bindings);
                        let out = self.eval_expr(&arm.body);
                        self.pop_frame();
                        return out;
                    }
                }

                Err(EvalError::no_matching_arm(fname, *span))
            }
        }
    }

    fn eval_bin(
        &mut self,
        span: Span,
        op: &BinOp,
        lhs: &Expr,
        rhs: &Expr,
    ) -> Result<Value, EvalError> {
        use Value::*;

        // short-circuiting boolean ops
        if *op == BinOp::And {
            let lval = self.eval_expr(lhs)?;
            let Bool(lb) = lval else {
                return Err(EvalError::type_error("boolean operands must be bool", span));
            };
            if !lb {
                return Ok(Bool(false));
            }
            let rval = self.eval_expr(rhs)?;
            let Bool(rb) = rval else {
                return Err(EvalError::type_error("boolean operands must be bool", span));
            };
            return Ok(Bool(rb));
        }
        if *op == BinOp::Or {
            let lval = self.eval_expr(lhs)?;
            let Bool(lb) = lval else {
                return Err(EvalError::type_error("boolean operands must be bool", span));
            };
            if lb {
                return Ok(Bool(true));
            }
            let rval = self.eval_expr(rhs)?;
            let Bool(rb) = rval else {
                return Err(EvalError::type_error("boolean operands must be bool", span));
            };
            return Ok(Bool(rb));
        }

        let l = self.eval_expr(lhs)?;
        let r = self.eval_expr(rhs)?;
        Ok(match (op, l, r) {
            (BinOp::Add, Int(a), Int(b)) => Int(a + b),
            (BinOp::Add, Float(a), Float(b)) => Float(a + b),
            (BinOp::Add, Int(a), Float(b)) => Float((a as f64) + b),
            (BinOp::Add, Float(a), Int(b)) => Float(a + (b as f64)),

            (BinOp::Sub, Int(a), Int(b)) => Int(a - b),
            (BinOp::Sub, Float(a), Float(b)) => Float(a - b),
            (BinOp::Sub, Int(a), Float(b)) => Float((a as f64) - b),
            (BinOp::Sub, Float(a), Int(b)) => Float(a - (b as f64)),

            (BinOp::Mul, Int(a), Int(b)) => Int(a * b),
            (BinOp::Mul, Float(a), Float(b)) => Float(a * b),
            (BinOp::Mul, Int(a), Float(b)) => Float((a as f64) * b),
            (BinOp::Mul, Float(a), Int(b)) => Float(a * (b as f64)),

            (BinOp::Div, Int(a), Int(b)) => {
                if b == 0 {
                    return Err(EvalError::divide_by_zero(span));
                }
                Int(a / b)
            }
            (BinOp::Div, Float(a), Float(b)) => {
                if b == 0.0 {
                    return Err(EvalError::divide_by_zero(span));
                }
                Float(a / b)
            }
            (BinOp::Div, Int(a), Float(b)) => {
                if b == 0.0 {
                    return Err(EvalError::divide_by_zero(span));
                }
                Float((a as f64) / b)
            }
            (BinOp::Div, Float(a), Int(b)) => {
                if b == 0 {
                    return Err(EvalError::divide_by_zero(span));
                }
                Float(a / (b as f64))
            }

            (BinOp::Mod, Int(a), Int(b)) => {
                if b == 0 {
                    return Err(EvalError::divide_by_zero(span));
                }
                Int(a % b)
            }

            (BinOp::BitAnd, Int(a), Int(b)) => Int(a & b),
            (BinOp::BitOr, Int(a), Int(b)) => Int(a | b),
            (BinOp::Xor, Int(a), Int(b)) => Int(a ^ b),

            (BinOp::Eq, a, b) => Bool(val_eq(&a, &b).map_err(|e| e.with_span(span))?),
            (BinOp::Ne, a, b) => Bool(!val_eq(&a, &b).map_err(|e| e.with_span(span))?),

            (BinOp::Lt, Int(a), Int(b)) => Bool(a < b),
            (BinOp::LtEq, Int(a), Int(b)) => Bool(a <= b),
            (BinOp::Gt, Int(a), Int(b)) => Bool(a > b),
            (BinOp::GtEq, Int(a), Int(b)) => Bool(a >= b),

            (BinOp::Lt, Float(a), Float(b)) => Bool(a < b),
            (BinOp::LtEq, Float(a), Float(b)) => Bool(a <= b),
            (BinOp::Gt, Float(a), Float(b)) => Bool(a > b),
            (BinOp::GtEq, Float(a), Float(b)) => Bool(a >= b),
            (BinOp::Lt, Int(a), Float(b)) => Bool((a as f64) < b),
            (BinOp::Lt, Float(a), Int(b)) => Bool(a < (b as f64)),
            (BinOp::LtEq, Int(a), Float(b)) => Bool((a as f64) <= b),
            (BinOp::LtEq, Float(a), Int(b)) => Bool(a <= (b as f64)),
            (BinOp::Gt, Int(a), Float(b)) => Bool((a as f64) > b),
            (BinOp::Gt, Float(a), Int(b)) => Bool(a > (b as f64)),
            (BinOp::GtEq, Int(a), Float(b)) => Bool((a as f64) >= b),
            (BinOp::GtEq, Float(a), Int(b)) => Bool(a >= (b as f64)),

            _ => return Err(EvalError::type_error("invalid operand types", span)),
        })
    }
}

// -------- pattern helpers --------

fn val_eq(a: &Value, b: &Value) -> Result<bool, EvalError> {
    use Value::*;
    Ok(match (a, b) {
        (Int(x), Int(y)) => x == y,
        (Float(x), Float(y)) => x == y,
        (Int(x), Float(y)) => (*x as f64) == *y,
        (Float(x), Int(y)) => *x == (*y as f64),
        (Bool(x), Bool(y)) => x == y,
        _ => false,
    })
}

/// Bind identifiers to argument values; literal patterns must equal.
/// Returns `Some(bindings)` if matched, else `None`.
fn match_and_bind(
    params: &[Pattern],
    args: &[Value],
    span: Span,
) -> Result<Option<HashMap<String, Value>>, EvalError> {
    let mut bindings = HashMap::new();
    for (p, a) in params.iter().zip(args) {
        match p {
            Pattern::Identifier(name) => {
                bindings.insert(name.clone(), a.clone());
            }
            Pattern::Lit(Literal::Int(n)) => {
                if !val_eq(a, &Value::Int(*n)).map_err(|e| e.with_span(span))? {
                    return Ok(None);
                }
            }
            Pattern::Lit(Literal::Float(x)) => {
                if !val_eq(a, &Value::Float(*x)).map_err(|e| e.with_span(span))? {
                    return Ok(None);
                }
            }
            Pattern::Lit(Literal::Bool(b)) => {
                if !val_eq(a, &Value::Bool(*b)).map_err(|e| e.with_span(span))? {
                    return Ok(None);
                }
            }
        }
    }
    Ok(Some(bindings))
}

/// Count literal parameters. Higher is "more specific".
fn pattern_specificity(params: &[Pattern]) -> usize {
    params
        .iter()
        .filter(|p| matches!(p, Pattern::Lit(_)))
        .count()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        lexer::{Lexer, token::Span},
        parser::{
            Parser,
            ast::{BinOp, Expr, FuncArm, Literal, Pattern, Stmt},
        },
    };
    use miette::SourceSpan;

    fn dummy_span() -> Span {
        Span::new(0, 0)
    }

    fn lit_int(n: i64) -> Expr {
        Expr::Lit(Literal::Int(n), dummy_span())
    }

    fn lit_bool(b: bool) -> Expr {
        Expr::Lit(Literal::Bool(b), dummy_span())
    }

    fn lit_float(f: f64) -> Expr {
        Expr::Lit(Literal::Float(f), dummy_span())
    }

    fn binary(lhs: Expr, op: BinOp, rhs: Expr) -> Expr {
        Expr::Binary {
            lhs: Box::new(lhs),
            op,
            span: dummy_span(),
            rhs: Box::new(rhs),
        }
    }

    fn eval_expr_stmt(env: &mut Env, expr: Expr) -> Result<Option<Value>, EvalError> {
        env.eval_stmt(&Stmt::Expression(expr))
    }

    fn call(name: &str, args: Vec<Expr>) -> Expr {
        Expr::Call {
            callee: Box::new(Expr::Identifier(name.into(), dummy_span())),
            args,
            span: dummy_span(),
        }
    }

    fn expect_value(env: &mut Env, expr: Expr) -> Value {
        eval_expr_stmt(env, expr)
            .expect("expression should evaluate successfully")
            .expect("expression statements should yield a value")
    }

    fn assert_float_eq(actual: Value, expected: f64) {
        match actual {
            Value::Float(v) => {
                assert!((v - expected).abs() < 1e-9, "expected {expected}, got {v}");
            }
            Value::Int(i) => {
                let v = i as f64;
                assert!(
                    (v - expected).abs() < 1e-9,
                    "expected float {expected}, got int {i}"
                );
            }
            other => panic!("expected numeric value, got {other:?}"),
        }
    }

    #[test]
    fn bool_ops_require_bool_operands() {
        let mut env = Env::new();
        let err = eval_expr_stmt(&mut env, binary(lit_int(1), BinOp::And, lit_bool(true)))
            .expect_err("expected type error for int && bool");
        assert!(
            matches!(
                err,
                EvalError::TypeError {
                    message: "boolean operands must be bool",
                    ..
                }
            ),
            "unexpected error: {err:?}"
        );

        let mut env = Env::new();
        let err = eval_expr_stmt(&mut env, binary(lit_bool(false), BinOp::Or, lit_int(0)))
            .expect_err("expected type error for bool || int");
        assert!(
            matches!(
                err,
                EvalError::TypeError {
                    message: "boolean operands must be bool",
                    ..
                }
            ),
            "unexpected error: {err:?}"
        );
    }

    #[test]
    fn bool_ops_short_circuit() {
        let mut env = Env::new();
        let rhs_err = binary(lit_int(1), BinOp::Div, lit_int(0));
        let expr = binary(lit_bool(false), BinOp::And, rhs_err);
        let result = eval_expr_stmt(&mut env, expr)
            .expect("expected short-circuit to avoid division by zero")
            .expect("expression statements should yield a value");
        assert_eq!(result, Value::Bool(false));

        let mut env = Env::new();
        let rhs_err = binary(lit_int(1), BinOp::Div, lit_int(0));
        let expr = binary(lit_bool(true), BinOp::Or, rhs_err);
        let result = eval_expr_stmt(&mut env, expr)
            .expect("expected short-circuit to avoid division by zero")
            .expect("expression statements should yield a value");
        assert_eq!(result, Value::Bool(true));
    }

    #[test]
    fn modulo_by_zero_reports_error() {
        let mut env = Env::new();
        let err = eval_expr_stmt(&mut env, binary(lit_int(5), BinOp::Mod, lit_int(0)))
            .expect_err("expected divide-by-zero error for modulo");
        assert!(matches!(err, EvalError::DivideByZero { .. }));
    }

    #[test]

    fn mixed_numeric_arithmetic() {
        let mut env = Env::new();

        assert_float_eq(
            expect_value(&mut env, binary(lit_int(1), BinOp::Add, lit_float(2.5))),
            3.5,
        );
        assert_float_eq(
            expect_value(&mut env, binary(lit_float(2.5), BinOp::Add, lit_int(1))),
            3.5,
        );

        assert_float_eq(
            expect_value(&mut env, binary(lit_int(1), BinOp::Sub, lit_float(2.5))),
            -1.5,
        );
        assert_float_eq(
            expect_value(&mut env, binary(lit_float(2.5), BinOp::Sub, lit_int(1))),
            1.5,
        );

        assert_float_eq(
            expect_value(&mut env, binary(lit_int(3), BinOp::Mul, lit_float(0.5))),
            1.5,
        );
        assert_float_eq(
            expect_value(&mut env, binary(lit_float(0.5), BinOp::Mul, lit_int(4))),
            2.0,
        );

        assert_float_eq(
            expect_value(&mut env, binary(lit_int(3), BinOp::Div, lit_float(2.0))),
            1.5,
        );
        assert_float_eq(
            expect_value(&mut env, binary(lit_float(3.0), BinOp::Div, lit_int(2))),
            1.5,
        );

        // Ensure int / float zero is caught
        let err = eval_expr_stmt(&mut env, binary(lit_int(1), BinOp::Div, lit_float(0.0)))
            .expect_err("expected divide-by-zero for int / float zero");
        assert!(matches!(err, EvalError::DivideByZero { .. }));
    }

    #[test]
    fn mixed_numeric_comparisons() {
        let mut env = Env::new();

        let lt = expect_value(&mut env, binary(lit_int(1), BinOp::Lt, lit_float(1.5)));
        assert_eq!(lt, Value::Bool(true));

        let gt = expect_value(&mut env, binary(lit_float(1.5), BinOp::Gt, lit_int(2)));
        assert_eq!(gt, Value::Bool(false));

        let ge = expect_value(&mut env, binary(lit_float(2.0), BinOp::GtEq, lit_int(2)));
        assert_eq!(ge, Value::Bool(true));

        let le = expect_value(&mut env, binary(lit_int(2), BinOp::LtEq, lit_float(2.0)));
        assert_eq!(le, Value::Bool(true));
    }

    #[test]
    fn literal_patterns_prefer_specific_arm() {
        let mut env = Env::new();
        // generic arm
        env.eval_stmt(&Stmt::FunctionDefinition {
            name: "f".into(),
            arms: vec![FuncArm {
                params: vec![Pattern::Identifier("x".into())],
                body: Expr::Identifier("x".into(), dummy_span()),
            }],
        })
        .unwrap();

        // literal-specific arm
        env.eval_stmt(&Stmt::FunctionDefinition {
            name: "f".into(),
            arms: vec![FuncArm {
                params: vec![Pattern::Lit(Literal::Int(1))],
                body: lit_int(42),
            }],
        })
        .unwrap();

        let result = eval_expr_stmt(&mut env, call("f", vec![lit_int(1)]))
            .unwrap()
            .unwrap();
        assert_eq!(result, Value::Int(42));

        let result = eval_expr_stmt(&mut env, call("f", vec![lit_int(2)]))
            .unwrap()
            .unwrap();
        assert_eq!(result, Value::Int(2));
    }

    #[test]
    fn undefined_variable_error_carries_span() {
        let mut env = Env::new();
        let expr = parse_expr_with_spans("x+1");
        let err = eval_expr_stmt(&mut env, expr).expect_err("expected undefined var error");
        match err {
            EvalError::UndefinedVar {
                name,
                span: Some(span),
            } => {
                assert_eq!(name, "x");
                assert_source_span(span, 0, 1);
            }
            other => panic!("unexpected error: {other:?}"),
        }
    }

    #[test]
    fn undefined_function_error_carries_span() {
        let mut env = Env::new();
        let expr = parse_expr_with_spans("foo()");
        let err = eval_expr_stmt(&mut env, expr).expect_err("expected undefined func error");
        match err {
            EvalError::UndefinedFunc {
                name,
                span: Some(span),
            } => {
                assert_eq!(name, "foo");
                assert_source_span(span, 0, 3);
            }
            other => panic!("unexpected error: {other:?}"),
        }
    }

    #[test]
    fn unary_type_error_reports_span() {
        let mut env = Env::new();
        let expr = parse_expr_with_spans("!1");
        let err = eval_expr_stmt(&mut env, expr).expect_err("expected type error");
        match err {
            EvalError::TypeError {
                message,
                span: Some(span),
            } => {
                assert_eq!(message, "invalid unary operand");
                assert_source_span(span, 0, 2);
            }
            other => panic!("unexpected error: {other:?}"),
        }
    }

    #[test]
    fn boolean_binop_type_error_reports_span() {
        let mut env = Env::new();
        let expr = parse_expr_with_spans("1&&true");
        let err = eval_expr_stmt(&mut env, expr).expect_err("expected type error");
        match err {
            EvalError::TypeError {
                message,
                span: Some(span),
            } => {
                assert_eq!(message, "boolean operands must be bool");
                assert_source_span(span, 0, 7);
            }
            other => panic!("unexpected error: {other:?}"),
        }
    }

    #[test]
    fn no_matching_arm_reports_span() {
        let mut env = Env::new();
        let definition = parse_stmt_with_spans("f(x, 1) = x");
        env.eval_stmt(&definition).expect("function registers");

        let expr = parse_expr_with_spans("f(0,0)");
        let err = eval_expr_stmt(&mut env, expr).expect_err("expected no matching arm");
        match err {
            EvalError::NoMatchingArm {
                name,
                span: Some(span),
            } => {
                assert_eq!(name, "f");
                assert_source_span(span, 0, 6);
            }
            other => panic!("unexpected error: {other:?}"),
        }
    }

    #[test]
    fn divide_by_zero_reports_span() {
        let mut env = Env::new();
        let expr = parse_expr_with_spans("1/0");
        let err = eval_expr_stmt(&mut env, expr).expect_err("expected divide by zero");
        match err {
            EvalError::DivideByZero { span: Some(span) } => {
                assert_source_span(span, 0, 3);
            }
            other => panic!("unexpected error: {other:?}"),
        }
    }

    fn parse_expr_with_spans(input: &str) -> Expr {
        match parse_stmt_with_spans(input) {
            Stmt::Expression(expr) => expr,
            other => panic!("expected expression statement, got {other:?}"),
        }
    }

    fn parse_stmt_with_spans(input: &str) -> Stmt {
        let mut parser = Parser::new(Lexer::new(input));
        parser.parse().expect("parse succeeds")
    }

    fn assert_source_span(span: SourceSpan, start: usize, len: usize) {
        let offset: usize = span.offset();
        assert_eq!(offset, start, "unexpected span offset");
        assert_eq!(span.len(), len, "unexpected span length");
    }
}
