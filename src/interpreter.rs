use crate::{
    common::{EthNum, EthString, Expr, ExprVisitor, Stmt, StmtVisitor, Token, TokenKind, Value},
    state::Environment,
};

use std::{
    cmp::{Ordering, PartialEq, PartialOrd},
    convert::{TryFrom, TryInto},
    fmt::{self, Display},
    ops::{Add, Div, Mul, Neg, Not, Sub},
};

#[derive(Debug, thiserror::Error)]
pub enum InterpreterError {
    #[error("Incorrect parameters for operator: {operator}")]
    Paramters { operator: &'static str },

    // needs to be a String and not &str because we don't have GATs yet :(
    #[error("Incorrect argument provided: {literal}")]
    Argument { literal: String },
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

impl Sub for Value {
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

impl Div for Value {
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
        if value.kind.is_literal() {
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

pub struct Interpreter<'source> {
    env: Environment<'source>,
}

impl<'source> Interpreter<'source> {
    pub fn interpret(&self, stmts: &'source [Stmt]) -> Result<(), InterpreterError> {
        for stmt in stmts {
            self.visit_stmt(stmt)?;
        }

        Ok(())
    }
}

impl<'source> ExprVisitor for Interpreter<'source> {
    type Out = Result<Value, InterpreterError>;

    fn visit_expr(&self, expr: &Expr) -> Result<Value, InterpreterError> {
        match expr {
            Expr::Literal(_) => self.visit_literal(expr),
            Expr::Binary(_) => self.visit_binary(expr),
            Expr::Group(_) => self.visit_group(expr),
            Expr::Unary(_) => self.visit_unary(expr),
        }
    }

    fn visit_binary(&self, expr: &Expr) -> Self::Out {
        if let Expr::Binary(bin) = expr {
            let left = self.visit_expr(&bin.left)?;
            let right = self.visit_expr(&bin.right)?;

            match bin.operator.0.kind {
                TokenKind::Plus => left + right,
                TokenKind::Minus => left - right,
                TokenKind::Star => left * right,
                TokenKind::Slash => left / right,
                TokenKind::Greater => Ok(Value::Bool(left > right)),
                TokenKind::GreaterEqual => Ok(Value::Bool(left >= right)),
                TokenKind::Less => Ok(Value::Bool(left < right)),
                TokenKind::LessEqual => Ok(Value::Bool(left <= right)),
                TokenKind::BangEqual => Ok(Value::Bool(!is_equal(&left, &right))),
                TokenKind::EqualEqual => Ok(Value::Bool(is_equal(&left, &right))),
                _ => Err(InterpreterError::Argument {
                    literal: format!("{}", bin.operator.0),
                }),
            }
        } else {
            Err(InterpreterError::Argument {
                literal: format!("{:?}", expr),
            })
        }
    }

    fn visit_literal(&self, expr: &Expr) -> Self::Out {
        if let Expr::Literal(e) = expr {
            e.try_into()
        } else {
            Err(InterpreterError::Argument {
                literal: format!("{:?}", expr),
            })
        }
    }

    fn visit_unary(&self, expr: &Expr) -> Self::Out {
        if let Expr::Unary(e) = expr {
            let literal = self.visit_literal(&e.right)?;
            match e.operator.kind {
                TokenKind::Minus => -literal,
                TokenKind::Bang => Ok(Value::Bool(!is_truthy(&literal))),
                _ => Err(InterpreterError::Argument {
                    literal: format!("{}", e.operator),
                }),
            }
        } else {
            Err(InterpreterError::Argument {
                literal: format!("{:?}", expr),
            })
        }
    }

    fn visit_group(&self, expr: &Expr) -> Self::Out {
        if let Expr::Group(e) = expr {
            self.visit_expr(e)
        } else {
            Err(InterpreterError::Argument {
                literal: format!("{:?}", expr),
            })
        }
    }
}

impl<'source> StmtVisitor for Interpreter<'source> {
    type Out = Result<(), InterpreterError>;

    fn visit_stmt(&self, stmt: &'source Stmt) -> Self::Out {
        match *stmt {
            Stmt::Expr(_) => self.visit_expr_stmt(stmt),
            Stmt::Print(_) => self.visit_print(stmt),
            Stmt::Var(_) => self.visit_var_stmt(stmt),
        }
    }

    fn visit_expr_stmt(&self, stmt: &'source Stmt) -> Self::Out {
        if let Stmt::Expr(e) = stmt {
            self.visit_expr(e)?;
        } else {
            return Err(InterpreterError::Argument {
                literal: format!("{:?}", stmt),
            });
        }

        Ok(())
    }

    fn visit_print(&self, stmt: &'source Stmt) -> Self::Out {
        if let Stmt::Print(e) = stmt {
            let value = self.visit_expr(e)?;
            println!("{}", value);
        } else {
            return Err(InterpreterError::Argument {
                literal: format!("{:?}", stmt),
            });
        }

        Ok(())
    }

    fn visit_var_stmt(&self, stmt: &'source Stmt) -> Self::Out {
        if let Stmt::Var(var) = stmt {
            if let Some(e) = var.init {
                let value = self.visit_expr(&e)?;
                self.env.define(&var.name, value);
            }
        } else {
            return Err(InterpreterError::Argument {
                literal: format!("{:?}", stmt),
            });
        }

        Ok(())
    }
}
