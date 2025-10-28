use std::collections::HashMap;
use std::fmt;

use crate::eval::error::EvalError;
use crate::parser::ast::*;

pub mod error;

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

#[derive(Debug, Default, Clone)]
pub struct Env {
    // scope stack: global frame at index 0; top is current frame
    scopes: Vec<HashMap<String, Value>>,
    // function name -> list of arms (pattern + body)
    funcs: HashMap<String, Vec<FuncArm>>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
            funcs: HashMap::new(),
        }
    }

    // ------------ scope helpers ------------
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

    // ------------ public API ------------
    pub fn eval_stmt(&mut self, s: &Stmt) -> Result<Option<Value>, EvalError> {
        match s {
            Stmt::Assignment { name, value } => {
                let v = self.eval_expr(value)?;
                self.set_var(name.clone(), v);
                Ok(None)
            }
            Stmt::FunctionDefinition { name, arms } => {
                self.funcs
                    .entry(name.clone())
                    .or_default()
                    .extend(arms.clone());
                Ok(None)
            }
            Stmt::Expression(e) => self.eval_expr(e).map(Some),
        }
    }

    // ------------ expressions ------------
    fn eval_expr(&mut self, e: &Expr) -> Result<Value, EvalError> {
        use Expr::*;
        match e {
            Lit(Literal::Int(n)) => Ok(Value::Int(*n)),
            Lit(Literal::Float(x)) => Ok(Value::Float(*x)),
            Lit(Literal::Bool(b)) => Ok(Value::Bool(*b)),

            Identifier(id) => self
                .get_var(id)
                .ok_or_else(|| EvalError::UndefinedVar(id.clone())),

            Group(inner) => self.eval_expr(inner),

            Unary { op, rhs } => {
                let v = self.eval_expr(rhs)?;
                match (op, v) {
                    (UnaryOp::Neg, Value::Int(i)) => Ok(Value::Int(-i)),
                    (UnaryOp::Neg, Value::Float(f)) => Ok(Value::Float(-f)),
                    (UnaryOp::Not, Value::Bool(b)) => Ok(Value::Bool(!b)),
                    _ => Err(EvalError::Type("invalid unary operand")),
                }
            }

            Binary { lhs, op, rhs } => self.eval_bin(op, lhs, rhs),

            Call { callee, args } => {
                // callee must be an identifier
                let fname: String = match &**callee {
                    Identifier(s) => s.clone(),
                    _ => return Err(EvalError::Type("callee must be identifier")),
                };

                // get arms and clone them to end the borrow
                let arms: Vec<FuncArm> = self
                    .funcs
                    .get(&fname)
                    .cloned()
                    .ok_or_else(|| EvalError::UndefinedFunc(fname.clone()))?;

                // evaluate arguments once
                let argv: Vec<Value> = args
                    .iter()
                    .map(|a| self.eval_expr(a))
                    .collect::<Result<_, _>>()?;

                // Option A: sort by specificity (literal patterns first)
                let mut sorted = arms;
                sorted.sort_by(|a, b| {
                    pattern_specificity(&b.params).cmp(&pattern_specificity(&a.params))
                });

                // try arms in order; first match wins
                for arm in &sorted {
                    if arm.params.len() != argv.len() {
                        continue;
                    }
                    if let Some(bindings) = match_and_bind(&arm.params, &argv)? {
                        self.push_frame(bindings);
                        let out = self.eval_expr(&arm.body);
                        self.pop_frame();
                        return out;
                    }
                }

                Err(EvalError::NoMatchingArm(fname))
            }
        }
    }

    fn eval_bin(&mut self, op: &BinOp, lhs: &Expr, rhs: &Expr) -> Result<Value, EvalError> {
        use Value::*;

        // short-circuiting boolean ops
        if *op == BinOp::And {
            let lb = matches!(self.eval_expr(lhs)?, Bool(true));
            return if !lb {
                Ok(Bool(false))
            } else {
                Ok(Bool(matches!(self.eval_expr(rhs)?, Bool(true))))
            };
        }
        if *op == BinOp::Or {
            let lb = matches!(self.eval_expr(lhs)?, Bool(true));
            return if lb {
                Ok(Bool(true))
            } else {
                Ok(Bool(matches!(self.eval_expr(rhs)?, Bool(true))))
            };
        }

        let l = self.eval_expr(lhs)?;
        let r = self.eval_expr(rhs)?;
        Ok(match (op, l, r) {
            (BinOp::Add, Int(a), Int(b)) => Int(a + b),
            (BinOp::Add, Float(a), Float(b)) => Float(a + b),

            (BinOp::Sub, Int(a), Int(b)) => Int(a - b),
            (BinOp::Sub, Float(a), Float(b)) => Float(a - b),

            (BinOp::Mul, Int(a), Int(b)) => Int(a * b),
            (BinOp::Mul, Float(a), Float(b)) => Float(a * b),

            (BinOp::Div, Int(a), Int(b)) => {
                if b == 0 {
                    return Err(EvalError::DivByZero);
                }
                Int(a / b)
            }
            (BinOp::Div, Float(a), Float(b)) => Float(a / b),

            (BinOp::Mod, Int(a), Int(b)) => Int(a % b),

            (BinOp::BitAnd, Int(a), Int(b)) => Int(a & b),
            (BinOp::BitOr, Int(a), Int(b)) => Int(a | b),
            (BinOp::Xor, Int(a), Int(b)) => Int(a ^ b),

            (BinOp::Eq, a, b) => Bool(val_eq(&a, &b)?),
            (BinOp::Ne, a, b) => Bool(!val_eq(&a, &b)?),

            (BinOp::Lt, Int(a), Int(b)) => Bool(a < b),
            (BinOp::LtEq, Int(a), Int(b)) => Bool(a <= b),
            (BinOp::Gt, Int(a), Int(b)) => Bool(a > b),
            (BinOp::GtEq, Int(a), Int(b)) => Bool(a >= b),

            (BinOp::Lt, Float(a), Float(b)) => Bool(a < b),
            (BinOp::LtEq, Float(a), Float(b)) => Bool(a <= b),
            (BinOp::Gt, Float(a), Float(b)) => Bool(a > b),
            (BinOp::GtEq, Float(a), Float(b)) => Bool(a >= b),

            _ => return Err(EvalError::Type("invalid operand types")),
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
) -> Result<Option<HashMap<String, Value>>, EvalError> {
    let mut bindings = HashMap::new();
    for (p, a) in params.iter().zip(args) {
        match p {
            Pattern::Identifier(name) => {
                bindings.insert(name.clone(), a.clone());
            }
            Pattern::Lit(Literal::Int(n)) => {
                if !val_eq(a, &Value::Int(*n))? {
                    return Ok(None);
                }
            }
            Pattern::Lit(Literal::Float(x)) => {
                if !val_eq(a, &Value::Float(*x))? {
                    return Ok(None);
                }
            }
            Pattern::Lit(Literal::Bool(b)) => {
                if !val_eq(a, &Value::Bool(*b))? {
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
