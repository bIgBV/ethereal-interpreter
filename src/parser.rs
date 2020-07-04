use thiserror::Error;

use crate::scanner::{Token, TokenKind};

use std::sync::atomic::{AtomicUsize, Ordering};

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Token),
    Group(Box<Expr>),
    Binary(Binary),
    Unary(Unary),
}

#[derive(Debug, Clone)]
pub struct Unary {
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Binary {
    left: Box<Expr>,
    operator: Operator,
    right: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Operator(Token);

#[derive(Debug)]
pub struct Parser<'a> {
    tokens: &'a [Token],
    current: AtomicUsize,
}

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("Reached end of file")]
    EOF,
    #[error("Unknown error occurred")]
    Unknown,

    #[error("{0}")]
    UserError(String),
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Parser {
            tokens,
            current: AtomicUsize::new(0),
        }
    }

    pub fn expression(&self) -> Result<Expr, ParseError> {
        self.equality()
    }

    fn equality(&self) -> Result<Expr, ParseError> {
        let mut expr = self.comparison();

        while self.match_kind(&[TokenKind::BangEqual, TokenKind::EqualEqual]) {
            let operator = self.previous()?;
            let right = self.comparison()?;
            expr = Ok(Expr::Binary(Binary {
                left: Box::new(expr?),
                operator: Operator(operator.clone()),
                right: Box::new(right),
            }));
        }

        expr
    }

    fn comparison(&self) -> Result<Expr, ParseError> {
        let mut expr = self.addition();

        while self.match_kind(&[
            TokenKind::Greater,
            TokenKind::GreaterEqual,
            TokenKind::Less,
            TokenKind::LessEqual,
        ]) {
            let operator = self.previous()?;
            let right = self.addition()?;
            expr = Ok(Expr::Binary(Binary {
                left: Box::new(expr?),
                operator: Operator(operator.clone()),
                right: Box::new(right),
            }));
        }

        expr
    }

    fn addition(&self) -> Result<Expr, ParseError> {
        let mut expr = self.multiplication();

        while self.match_kind(&[TokenKind::Minus, TokenKind::Plus]) {
            let operator = self.previous()?;
            let right = self.multiplication()?;
            expr = Ok(Expr::Binary(Binary {
                left: Box::new(expr?),
                operator: Operator(operator.clone()),
                right: Box::new(right),
            }));
        }

        expr
    }

    fn multiplication(&self) -> Result<Expr, ParseError> {
        let mut expr = self.unary();

        while self.match_kind(&[TokenKind::Slash, TokenKind::Star]) {
            let operator = self.previous()?;
            let right = self.unary()?;
            expr = Ok(Expr::Binary(Binary {
                left: Box::new(expr?),
                operator: Operator(operator.clone()),
                right: Box::new(right),
            }));
        }

        expr
    }

    fn unary(&self) -> Result<Expr, ParseError> {
        if self.match_kind(&[TokenKind::Bang, TokenKind::Minus]) {
            let operator_token = self.previous()?;
            let right = self.unary()?;
            return Ok(Expr::Unary(Unary {
                operator: operator_token.clone(),
                right: Box::new(right),
            }));
        }

        self.primary()
    }

    fn primary(&self) -> Result<Expr, ParseError> {
        match self.peek().ok_or(ParseError::EOF)?.kind {
            TokenKind::False
            | TokenKind::True
            | TokenKind::Nil
            | TokenKind::Number(_)
            | TokenKind::Str(_) => Ok(Expr::Literal(self.previous()?.clone())),
            TokenKind::LeftParen => {
                let expr = self.expression()?;
                self.consume(&TokenKind::RightParen, "Expected ')' after expression")?;
                Ok(Expr::Group(Box::new(expr)))
            }
            _ => Err(ParseError::Unknown),
        }
    }

    fn match_kind(&self, kinds: &[TokenKind]) -> bool {
        for kind in kinds {
            if self.check(kind) {
                if self.advance().is_err() {
                    return false;
                } else {
                    return true;
                }
            }
        }

        false
    }

    fn consume(&self, kind: &TokenKind, message: &str) -> Result<&Token, ParseError> {
        if self.check(kind) {
            return self.advance();
        }

        match self.peek().ok_or(ParseError::EOF)? {
            val => {
                if val.kind == TokenKind::EOF {
                    Err(ParseError::EOF)
                } else {
                    Err(ParseError::UserError(format!("at {}", message)))
                }
            }
        }
    }

    fn synchronize(&self) -> Result<(), ParseError> {
        self.advance()?;

        loop {
            if self.is_at_end() {
                return Err(ParseError::EOF);
            }

            if self.previous()?.kind == TokenKind::Semicolon {
                return Ok(());
            }

            match self.peek().ok_or(ParseError::EOF)?.kind {
                TokenKind::Class
                | TokenKind::Fun
                | TokenKind::Var
                | TokenKind::For
                | TokenKind::If
                | TokenKind::While
                | TokenKind::Print
                | TokenKind::Return => return Ok(()),
                _ => self.advance()?,
            };
        }
    }

    fn check(&self, kind: &TokenKind) -> bool {
        if self.is_at_end() {
            false
        } else {
            self.peek().map(|v| v.kind == *kind).unwrap_or(false)
        }
    }

    fn advance(&self) -> Result<&Token, ParseError> {
        if !self.is_at_end() {
            self.current.fetch_add(1, Ordering::Relaxed);
        }

        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.peek()
            .map(|v| v.kind == TokenKind::EOF)
            .unwrap_or(false)
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.current.load(Ordering::Relaxed))
    }

    fn previous(&self) -> Result<&Token, ParseError> {
        self.tokens
            .get(self.current.fetch_sub(1, Ordering::Relaxed))
            .ok_or(ParseError::EOF)
    }
}

pub trait Visitor<T> {
    fn visit_binary(&self, expr: &Expr) -> T;
    fn visit_literal(&self, expr: &Expr) -> T;
    fn visit_unary(&self, expr: &Expr) -> T;
    fn visit_group(&self, expr: &Expr) -> T;
}
