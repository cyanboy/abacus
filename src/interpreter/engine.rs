use std::{collections::HashMap, rc::Rc};

use crate::{
    lexer::token::Span,
    parser::ast::{BinOp, Expr, FuncArm, Literal, Pattern, Stmt, UnaryOp},
};

use super::{error::EvalError, values::Value};

#[derive(Debug)]
pub struct Env {
    // scope stack: global frame at index 0; top is current frame
    scopes: Vec<HashMap<String, Value>>,
    // function name -> list of arms (pattern + body) ordered by specificity
    funcs: HashMap<String, Vec<FuncEntry>>,
}

#[derive(Debug)]
struct FuncEntry {
    arm: Rc<FuncArm>,
    specificity: usize,
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
                for arm in arms.iter() {
                    let specificity = pattern_specificity(&arm.params);
                    let rc = Rc::new(arm.clone());

                    if let Some(existing_idx) = entry
                        .iter()
                        .position(|existing| existing.arm.params == arm.params)
                    {
                        entry[existing_idx] = FuncEntry {
                            arm: rc,
                            specificity,
                        };
                        continue;
                    }

                    let insert_idx = entry
                        .iter()
                        .position(|existing| existing.specificity < specificity)
                        .unwrap_or(entry.len());
                    entry.insert(
                        insert_idx,
                        FuncEntry {
                            arm: rc,
                            specificity,
                        },
                    );
                }
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

                let arms: Vec<Rc<FuncArm>> = match self.funcs.get(&fname) {
                    Some(entries) => entries.iter().map(|entry| Rc::clone(&entry.arm)).collect(),
                    None => return Err(EvalError::undefined_func(fname.clone(), fname_span)),
                };

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

fn val_eq(a: &Value, b: &Value) -> Result<bool, EvalError> {
    use Value::*;
    Ok(match (a, b) {
        (Int(x), Int(y)) => x == y,
        (Float(x), Float(y)) => x == y,
        (Int(x), Float(y)) => (*x as f64) == *y,
        (Float(x), Int(y)) => *x == (*y as f64),
        (Bool(x), Bool(y)) => x == y,
        _ => {
            return Err(EvalError::TypeError {
                message: "operands must be comparable",
                span: None,
            });
        }
    })
}

/// Bind identifiers to argument values; literal patterns must equal.
/// Returns `Some(bindings)` if matched, else `None`.
fn match_and_bind(
    params: &[Pattern],
    args: &[Value],
    _span: Span,
) -> Result<Option<HashMap<String, Value>>, EvalError> {
    let mut bindings = HashMap::new();
    for (p, a) in params.iter().zip(args) {
        match p {
            Pattern::Identifier(name) => {
                if let Some(existing) = bindings.get(name) {
                    match val_eq(existing, a) {
                        Ok(true) => {}
                        Ok(false) | Err(_) => return Ok(None),
                    }
                } else {
                    bindings.insert(name.clone(), a.clone());
                }
            }
            Pattern::Lit(Literal::Int(n)) => match val_eq(a, &Value::Int(*n)) {
                Ok(true) => {}
                Ok(false) | Err(_) => return Ok(None),
            },
            Pattern::Lit(Literal::Float(x)) => match val_eq(a, &Value::Float(*x)) {
                Ok(true) => {}
                Ok(false) | Err(_) => return Ok(None),
            },
            Pattern::Lit(Literal::Bool(b)) => match val_eq(a, &Value::Bool(*b)) {
                Ok(true) => {}
                Ok(false) | Err(_) => return Ok(None),
            },
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
        lexer::Lexer,
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

    fn ident(name: &str) -> Expr {
        Expr::Identifier(name.into(), dummy_span())
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
    fn bitwise_ops_on_integers() {
        let mut env = Env::new();
        let and = expect_value(&mut env, binary(lit_int(6), BinOp::BitAnd, lit_int(3)));
        assert_eq!(and, Value::Int(2));

        let or = expect_value(&mut env, binary(lit_int(4), BinOp::BitOr, lit_int(1)));
        assert_eq!(or, Value::Int(5));

        let xor = expect_value(&mut env, binary(lit_int(10), BinOp::Xor, lit_int(15)));
        assert_eq!(xor, Value::Int(5));
    }

    #[test]
    fn literal_bool_and_float_patterns_match_correctly() {
        let mut env = Env::new();
        let arms = vec![
            FuncArm {
                params: vec![Pattern::Lit(Literal::Bool(true))],
                body: lit_int(1),
            },
            FuncArm {
                params: vec![Pattern::Lit(Literal::Float(3.14))],
                body: lit_int(2),
            },
        ];
        let func = Stmt::FunctionDefinition {
            name: "check".into(),
            arms,
        };
        env.eval_stmt(&func).unwrap();

        let result_true = expect_value(
            &mut env,
            call("check", vec![Expr::Lit(Literal::Bool(true), dummy_span())]),
        );
        assert_eq!(result_true, Value::Int(1));

        let result_float = expect_value(
            &mut env,
            call("check", vec![Expr::Lit(Literal::Float(3.14), dummy_span())]),
        );
        assert_eq!(result_float, Value::Int(2));

        let no_match = env.eval_stmt(&Stmt::Expression(call(
            "check",
            vec![Expr::Lit(Literal::Bool(false), dummy_span())],
        )));
        assert!(matches!(no_match, Err(EvalError::NoMatchingArm { .. })));
    }

    #[test]
    fn mixed_numeric_equality_and_comparison() {
        let mut env = Env::new();
        let eq_val = expect_value(
            &mut env,
            binary(
                Expr::Lit(Literal::Int(5), dummy_span()),
                BinOp::Eq,
                Expr::Lit(Literal::Float(5.0), dummy_span()),
            ),
        );
        assert_eq!(eq_val, Value::Bool(true));

        let lt_val = expect_value(
            &mut env,
            binary(
                Expr::Lit(Literal::Float(4.5), dummy_span()),
                BinOp::Lt,
                Expr::Lit(Literal::Int(5), dummy_span()),
            ),
        );
        assert_eq!(lt_val, Value::Bool(true));
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
        env.eval_stmt(&Stmt::FunctionDefinition {
            name: "f".into(),
            arms: vec![FuncArm {
                params: vec![Pattern::Identifier("x".into())],
                body: ident("x"),
            }],
        })
        .unwrap();

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
    fn equality_type_error_for_incompatible_types() {
        let mut env = Env::new();
        let expr = parse_expr_with_spans("1==true");
        let err = eval_expr_stmt(&mut env, expr).expect_err("expected type error");
        match err {
            EvalError::TypeError {
                message,
                span: Some(span),
            } => {
                assert_eq!(message, "operands must be comparable");
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
    fn repeated_identifier_pattern_requires_equal_arguments() {
        let mut env = Env::new();
        let definition = parse_stmt_with_spans("f(x, x) = x");
        env.eval_stmt(&definition).expect("function registers");

        let value = expect_value(&mut env, parse_expr_with_spans("f(5,5)"));
        assert_eq!(value, Value::Int(5));

        let err = eval_expr_stmt(&mut env, parse_expr_with_spans("f(5,6)"))
            .expect_err("expected mismatch to reject arm");
        assert!(
            matches!(err, EvalError::NoMatchingArm { .. }),
            "unexpected error: {err:?}"
        );
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

    #[test]
    fn redefining_function_replaces_existing_arm() {
        let mut env = Env::new();
        let def1 = parse_stmt_with_spans("f(x) = x + 1");
        env.eval_stmt(&def1).expect("function registers");

        let value = expect_value(&mut env, parse_expr_with_spans("f(5)"));
        assert_eq!(value, Value::Int(6));

        let def2 = parse_stmt_with_spans("f(x) = x * 2");
        env.eval_stmt(&def2)
            .expect("function redefinition succeeds");

        let value = expect_value(&mut env, parse_expr_with_spans("f(5)"));
        assert_eq!(value, Value::Int(10));
    }

    #[test]
    fn callee_must_be_identifier() {
        let mut env = Env::new();
        let expr = Expr::Call {
            callee: Box::new(lit_int(5)),
            args: vec![lit_int(1)],
            span: dummy_span(),
        };
        let err = env
            .eval_stmt(&Stmt::Expression(expr))
            .expect_err("call with non-identifier callee should fail");
        match err {
            EvalError::TypeError { message, .. } => {
                assert_eq!(message, "callee must be identifier")
            }
            other => panic!("unexpected error {other:?}"),
        }
    }

    #[test]
    fn match_and_bind_allows_repeated_identifier_when_values_match() {
        let bindings = match_and_bind(
            &[
                Pattern::Identifier("x".into()),
                Pattern::Identifier("x".into()),
            ],
            &[Value::Int(7), Value::Int(7)],
            dummy_span(),
        )
        .expect("matching should not raise errors")
        .expect("patterns should match");
        assert_eq!(bindings.get("x"), Some(&Value::Int(7)));
    }

    #[test]
    fn val_eq_returns_false_for_mismatched_types() {
        let err = val_eq(&Value::Bool(true), &Value::Int(1)).expect_err("should be type error");
        assert!(matches!(err, EvalError::TypeError { .. }));
    }

    #[test]
    fn pattern_specificity_counts_literal_params() {
        let specific = pattern_specificity(&[
            Pattern::Identifier("a".into()),
            Pattern::Lit(Literal::Int(1)),
            Pattern::Lit(Literal::Bool(false)),
        ]);
        assert_eq!(specific, 2);
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
