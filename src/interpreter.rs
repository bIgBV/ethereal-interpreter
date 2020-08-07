use anyhow::Error;

use crate::{
    common::{EthNum, EthString, Expr, ExprVisitor, Stmt, StmtVisitor, Token, TokenKind, Value},
    environment::Environment,
};

use std::{
    cell::RefCell,
    cmp::{Ordering, PartialEq, PartialOrd},
    convert::{AsRef, TryFrom, TryInto},
    fmt::{self, Display},
    ops::{Add, Div, Mul, Neg, Not, Sub},
    rc::Rc,
};

#[derive(Debug, thiserror::Error)]
pub enum InterpreterError {
    #[error("Incorrect parameters for operator: {operator}")]
    Paramters { operator: &'static str },

    // needs to be a String and not &str because we don't have GATs yet :(
    #[error("Incorrect argument provided: {literal}")]
    Argument { literal: String },

    #[error("Undefined variable {name}.")]
    UndefinedVar { name: String },
}

impl Add for Value {
    type Output = Result<Value, InterpreterError>;

    fn add(self, other: Self) -> Self::Output {
        match (self, other) {
            (Value::Num(lhs), Value::Num(rhs)) => Ok(Value::Num(EthNum(lhs.0 + rhs.0))),
            (Value::Str(lhs), Value::Str(rhs)) => {
                Ok(Value::Str(EthString(format!("{}{}", lhs.0, rhs.0))))
            }
            _ => Err(InterpreterError::Paramters { operator: "+" }),
        }
    }
}

impl Add for &Value {
    type Output = Result<Value, InterpreterError>;

    fn add(self, other: Self) -> Self::Output {
        match (self, other) {
            (Value::Num(lhs), Value::Num(rhs)) => Ok(Value::Num(EthNum(lhs.0 + rhs.0))),
            (Value::Str(lhs), Value::Str(rhs)) => {
                Ok(Value::Str(EthString(format!("{}{}", lhs.0, rhs.0))))
            }
            _ => Err(InterpreterError::Paramters { operator: "+" }),
        }
    }
}

impl Sub for Value {
    type Output = Result<Value, InterpreterError>;

    fn sub(self, other: Self) -> Self::Output {
        match (self, other) {
            (Value::Num(lhs), Value::Num(rhs)) => Ok(Value::Num(EthNum(lhs.0 - rhs.0))),
            _ => Err(InterpreterError::Paramters { operator: "-" }),
        }
    }
}

impl Sub for &Value {
    type Output = Result<Value, InterpreterError>;

    fn sub(self, other: Self) -> Self::Output {
        match (self, other) {
            (Value::Num(lhs), Value::Num(rhs)) => Ok(Value::Num(EthNum(lhs.0 - rhs.0))),
            _ => Err(InterpreterError::Paramters { operator: "-" }),
        }
    }
}

impl Mul for Value {
    type Output = Result<Value, InterpreterError>;

    fn mul(self, other: Self) -> Self::Output {
        match (self, other) {
            (Value::Num(lhs), Value::Num(rhs)) => Ok(Value::Num(EthNum(lhs.0 * rhs.0))),
            _ => Err(InterpreterError::Paramters { operator: "*" }),
        }
    }
}

impl Mul for &Value {
    type Output = Result<Value, InterpreterError>;

    fn mul(self, other: Self) -> Self::Output {
        match (self, other) {
            (Value::Num(lhs), Value::Num(rhs)) => Ok(Value::Num(EthNum(lhs.0 * rhs.0))),
            _ => Err(InterpreterError::Paramters { operator: "*" }),
        }
    }
}

impl Div for Value {
    type Output = Result<Value, InterpreterError>;

    fn div(self, other: Self) -> Self::Output {
        match (self, other) {
            (Value::Num(lhs), Value::Num(rhs)) => Ok(Value::Num(EthNum(lhs.0 / rhs.0))),
            _ => Err(InterpreterError::Paramters { operator: "/" }),
        }
    }
}

impl Div for &Value {
    type Output = Result<Value, InterpreterError>;

    fn div(self, other: Self) -> Self::Output {
        match (self, other) {
            (Value::Num(lhs), Value::Num(rhs)) => Ok(Value::Num(EthNum(lhs.0 / rhs.0))),
            _ => Err(InterpreterError::Paramters { operator: "/" }),
        }
    }
}

impl Neg for Value {
    type Output = Result<Value, InterpreterError>;

    fn neg(self) -> Self::Output {
        match self {
            Value::Num(n) => Ok(Value::Num(EthNum(-n.0))),
            _ => Err(InterpreterError::Paramters { operator: "-" }),
        }
    }
}

impl Neg for &Value {
    type Output = Result<Value, InterpreterError>;

    fn neg(self) -> Self::Output {
        match self {
            Value::Num(n) => Ok(Value::Num(EthNum(-n.0))),
            _ => Err(InterpreterError::Paramters { operator: "-" }),
        }
    }
}

impl Not for Value {
    type Output = Result<Value, InterpreterError>;

    fn not(self) -> Self::Output {
        match self {
            Value::Bool(b) => Ok(Value::Bool(!b)),
            _ => Err(InterpreterError::Paramters { operator: "!" }),
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Value) -> Option<Ordering> {
        if let (Value::Num(lhs), Value::Num(rhs)) = (self, other) {
            lhs.partial_cmp(rhs)
        } else {
            None
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Num(lhs), Value::Num(rhs)) => lhs == rhs,
            (Value::Bool(lhs), Value::Bool(rhs)) => lhs == rhs,
            _ => false,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Num(n) => write!(f, "{}", n),
            Value::Str(s) => write!(f, "{}", s),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug)]
pub enum Output<T> {
    Val(T),
    Ref(Rc<T>),
}

impl<T> Output<T> {
    fn map_with<F, U>(self, other: Output<T>, f: F) -> U
    where
        F: FnOnce(&T, &T) -> U,
    {
        match (self, other) {
            (Output::Val(l), Output::Val(r)) => f(&l, &r),
            (Output::Ref(l), Output::Ref(r)) => f(l.as_ref(), r.as_ref()),
            (Output::Val(l), Output::Ref(r)) => f(&l, r.as_ref()),
            (Output::Ref(l), Output::Val(r)) => f(l.as_ref(), &r),
        }
    }

    fn map<F, U>(self, f: F) -> U
    where
        F: FnOnce(&T) -> U,
    {
        match self {
            Output::Val(v) => f(&v),
            Output::Ref(v) => f(v.as_ref()),
        }
    }
}

impl<T> AsRef<T> for Output<T> {
    fn as_ref(&self) -> &T {
        match self {
            Output::Val(v) => &v,
            Output::Ref(rc) => rc.as_ref(),
        }
    }
}

impl From<Value> for Output<Value> {
    fn from(v: Value) -> Self {
        Output::Val(v)
    }
}

impl From<Rc<Value>> for Output<Value> {
    fn from(v: Rc<Value>) -> Self {
        Output::Ref(v)
    }
}

impl<T> Display for Output<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Output::Val(v) => write!(f, "{}", v),
            Output::Ref(v) => write!(f, "{}", *v),
        }
    }
}

fn is_truthy(val: &Value) -> bool {
    match val {
        Value::Nil => false,
        Value::Bool(b) => *b,
        _ => true,
    }
}

fn is_equal(lhs: &Value, rhs: &Value) -> bool {
    if let (Value::Nil, Value::Nil) = (lhs, rhs) {
        return true;
    }

    if let (Value::Nil, _) = (lhs, rhs) {
        return false;
    }

    lhs == rhs
}

impl TryFrom<&Token> for Value {
    type Error = InterpreterError;

    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        if value.kind.is_primary() {
            match &value.kind {
                // fuck it, let's just clone things.
                TokenKind::Number(num) => Ok(Value::Num(num.clone())),
                TokenKind::Str(string) => Ok(Value::Str(string.clone())),
                TokenKind::True => Ok(Value::Bool(true)),
                TokenKind::False => Ok(Value::Bool(false)),
                TokenKind::Nil => Ok(Value::Nil),
                _ => Err(InterpreterError::Argument {
                    literal: format!("{}", value),
                }),
            }
        } else {
            Err(InterpreterError::Argument {
                literal: format!("{}", value),
            })
        }
    }
}

pub struct Interpreter {
    env: Environment,
    current: RefCell<usize>,
}

impl Interpreter {
    pub fn new() -> Self {
        let (env, current) = Environment::new();
        Interpreter {
            env,
            current: RefCell::new(current),
        }
    }

    pub fn interpret(&self, stmts: &[Stmt]) -> Result<(), Error> {
        for stmt in stmts {
            self.visit_stmt(stmt)?;
        }

        Ok(())
    }
}

pub type VisitorResult = Result<Output<Value>, Error>;

impl ExprVisitor<VisitorResult> for Interpreter {
    fn visit_binary(&self, expr: &Expr) -> VisitorResult {
        if let Expr::Binary(bin) = expr {
            let left = self.visit_expr(&bin.left)?;
            let right = self.visit_expr(&bin.right)?;

            match bin.operator.0.kind {
                TokenKind::Plus => Ok(left.map_with(right, |l, r| l + r)?.into()),
                TokenKind::Minus => Ok(left.map_with(right, |l, r| l - r)?.into()),
                TokenKind::Star => Ok(left.map_with(right, |l, r| l * r)?.into()),
                TokenKind::Slash => Ok(left.map_with(right, |l, r| l / r)?.into()),
                TokenKind::Greater => Ok(left.map_with(right, |l, r| Value::Bool(l > r)).into()),
                TokenKind::Less => Ok(left.map_with(right, |l, r| Value::Bool(l < r)).into()),
                TokenKind::LessEqual => Ok(left.map_with(right, |l, r| Value::Bool(l <= r)).into()),
                TokenKind::BangEqual => {
                    Ok(left.map_with(right, |l, r| Value::Bool(!is_equal(&l, &r)).into()))
                }
                TokenKind::EqualEqual => {
                    Ok(left.map_with(right, |l, r| Value::Bool(is_equal(&l, &r)).into()))
                }
                _ => Err(InterpreterError::Argument {
                    literal: format!("{}", bin.operator.0),
                }
                .into()),
            }
        } else {
            Err(InterpreterError::Argument {
                literal: format!("{:?}", expr),
            }
            .into())
        }
    }

    fn visit_literal(&self, expr: &Expr) -> VisitorResult {
        if let Expr::Literal(e) = expr {
            Ok(Output::Val(e.try_into()?))
        } else {
            Err(InterpreterError::Argument {
                literal: format!("{:?}", expr),
            }
            .into())
        }
    }

    fn visit_unary(&self, expr: &Expr) -> VisitorResult {
        if let Expr::Unary(e) = expr {
            let literal = self.visit_literal(&e.right)?;
            match e.operator.kind {
                TokenKind::Minus => Ok(literal.map(|v| -v)?.into()),
                TokenKind::Bang => Ok(literal.map(|v| Value::Bool(!is_truthy(&v))).into()),
                _ => Err(InterpreterError::Argument {
                    literal: format!("{}", e.operator),
                }
                .into()),
            }
        } else {
            Err(InterpreterError::Argument {
                literal: format!("{:?}", expr),
            }
            .into())
        }
    }

    fn visit_group(&self, expr: &Expr) -> VisitorResult {
        if let Expr::Group(e) = expr {
            self.visit_expr(e)
        } else {
            Err(InterpreterError::Argument {
                literal: format!("{:?}", expr),
            }
            .into())
        }
    }

    fn visit_var(&self, expr: &Expr) -> VisitorResult {
        if let Expr::Variable(var) = expr {
            self.env
                .get(self.current.borrow().clone(), var.lexeme.as_str())
                .map(|val| val.into())
                .map_err(|e| e.into())
        } else {
            Err(InterpreterError::Argument {
                literal: format!("{:?}", expr),
            }
            .into())
        }
    }

    fn visit_assign(&self, expr: &Expr) -> VisitorResult {
        if let Expr::Assign(e) = expr {
            let value = self.visit_expr(&e.value)?;

            if let Output::Val(val) = value {
                self.env
                    .assign(self.current.borrow().clone(), e.name.lexeme.clone(), val)?;

                self.env
                    .get(self.current.borrow().clone(), e.name.lexeme.as_str())
                    .map(|val| val.into())
                    .map_err(|e| e.into())
            } else {
                panic!("Trying to assign an already assigned varaible")
            }
        } else {
            Err(InterpreterError::Argument {
                literal: format!("{:?}", expr),
            }
            .into())
        }
    }

    fn visit_logical(&self, expr: &Expr) -> VisitorResult {
        if let Expr::Logical(logical) = expr {
            let left = self.visit_expr(&logical.left)?;

            if logical.operator.0.kind == TokenKind::Or {
                if is_truthy(left.as_ref()) {
                    return Ok(left);
                }
            } else {
                if !is_truthy(left.as_ref()) {
                    return Ok(left);
                }
            }

            self.visit_expr(&logical.right)
        } else {
            Err(InterpreterError::Argument {
                literal: format!("{:?}", expr),
            }
            .into())
        }
    }
}

type StmtResult = Result<(), Error>;

impl StmtVisitor<StmtResult> for Interpreter {
    fn visit_expr_stmt(&self, stmt: &Stmt) -> StmtResult {
        if let Stmt::Expr(e) = stmt {
            self.visit_expr(e)?;
        } else {
            return Err(InterpreterError::Argument {
                literal: format!("{:?}", stmt),
            }
            .into());
        }

        Ok(())
    }

    fn visit_print(&self, stmt: &Stmt) -> StmtResult {
        if let Stmt::Print(e) = stmt {
            let value = self.visit_expr(e)?;
            println!("{}", value);
        } else {
            return Err(InterpreterError::Argument {
                literal: format!("{:?}", stmt),
            }
            .into());
        }

        Ok(())
    }

    fn visit_var_stmt(&self, stmt: &Stmt) -> StmtResult {
        if let Stmt::Var(var) = stmt {
            if let Some(e) = &var.init {
                let value = self.visit_expr(&e)?;
                match value {
                    Output::Val(v) => {
                        self.env
                            .define(self.current.borrow().clone(), var.name.lexeme.clone(), v)
                    }
                    Output::Ref(_) => panic!("Trying to re-insert an inserted value"),
                };
            }
        } else {
            return Err(InterpreterError::Argument {
                literal: format!("{:?}", stmt),
            }
            .into());
        }

        Ok(())
    }

    fn visit_block_stmt(&self, stmt: &Stmt) -> StmtResult {
        if let Stmt::Block(stmts) = stmt {
            // Store the current scope
            let previous = self.current.borrow().clone();

            // Instantiate and assign new scope
            let new_scope = self.env.instantiate_new_scope(Some(previous));
            *self.current.borrow_mut() = new_scope;

            let result: StmtResult = {
                for statement in stmts {
                    self.visit_stmt(statement)?;
                }

                Ok(())
            };

            self.env.drop_scope(self.current.borrow().clone());

            // restore previous scope
            *self.current.borrow_mut() = previous;

            return result;
        }

        Ok(())
    }

    fn visit_if_stmt(&self, stmt: &Stmt) -> StmtResult {
        if let Stmt::If(if_stmt) = stmt {
            if is_truthy(self.visit_expr(&if_stmt.cond)?.as_ref()) {
                return self.visit_stmt(&if_stmt.then);
            }

            if let Some(unless) = &*if_stmt.unless {
                return self.visit_stmt(&unless);
            }
        } else {
            return Err(InterpreterError::Argument {
                literal: format!("{:?}", stmt),
            }
            .into());
        }

        Ok(())
    }

    fn visit_while_stmt(&self, stmt: &Stmt) -> StmtResult {
        if let Stmt::While(while_stmt) = stmt {
            while is_truthy(self.visit_expr(&while_stmt.cond)?.as_ref()) {
                self.visit_stmt(&while_stmt.body)?;
            }
        } else {
            return Err(InterpreterError::Argument {
                literal: format!("{:?}", stmt),
            }
            .into());
        }

        Ok(())
    }
}
