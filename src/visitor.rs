use crate::scanner::Token;

#[derive(Debug)]
pub enum Expr {
    Literal(Token),
    Group(Box<Expr>),
    Binary(Binary),
    Unary(Unary),
}

#[derive(Debug)]
pub struct Unary {
    pub operator: Token,
    pub right: Box<Expr>,
}
#[derive(Debug)]
pub struct Binary {
    left: Box<Expr>,
    operator: Operator,
    right: Box<Expr>,
}

#[derive(Debug)]
pub struct Operator(Token);

pub trait Visitor<T> {
    fn visit_binary(&self, expr: &Expr) -> T;
    fn visit_literal(&self, expr: &Expr) -> T;
    fn visit_unary(&self, expr: &Expr) -> T;
    fn visit_group(&self, expr: &Expr) -> T;
}
