use thiserror::Error;

use crate::common::{Binary, Expr, Operator, Token, TokenKind, Unary};

use std::sync::atomic::{AtomicUsize, Ordering};

#[derive(Debug, Error)]
pub enum ParseError<'a> {
    #[error("Reached end of file")]
    EOF,
    #[error("Unknown error occurred")]
    Unknown,

    #[error("Error occurred at{token} , {message}")]
    UserError { token: &'a Token, message: String },
}

#[derive(Debug)]
pub struct Parser<'a> {
    tokens: &'a [Token],
    current: AtomicUsize,
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
            let operator = self.previous();
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
            let operator = self.previous();
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
            let operator = self.previous();
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
            let operator = self.previous();
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
            let operator_token = self.previous();
            let right = self.unary()?;
            return Ok(Expr::Unary(Unary {
                operator: operator_token.clone(),
                right: Box::new(right),
            }));
        }

        self.primary()
    }

    fn primary(&self) -> Result<Expr, ParseError> {
        // Kind of an ad-hoc `match_kind` as we need to flip the match becasue Number and Str
        // variants contain values.
        if self.peek()?.kind.is_primary() {
            // call advance to increment the cursor and return the previous token
            Ok(Expr::Literal(self.advance().clone()))
        } else if self.match_kind(&[TokenKind::LeftParen]) {
            let expr = self.expression()?;
            self.consume(&TokenKind::RightParen, "Expected ')' after expression")?;
            Ok(Expr::Group(Box::new(expr)))
        } else {
            Err(ParseError::UserError {
                token: self.peek()?,
                message: String::from("Expected expression"),
            })
        }
    }

    fn match_kind(&self, kinds: &[TokenKind]) -> bool {
        let result = kinds.iter().any(|tok| self.check(tok));

        if result {
            self.advance();
        }

        result
    }

    fn consume(&self, kind: &TokenKind, message: &str) -> Result<&Token, ParseError> {
        if self.check(kind) {
            return Ok(self.advance());
        }

        let token = self.peek()?;
        if let TokenKind::EOF = token.kind {
            Err(ParseError::EOF)
        } else {
            Err(ParseError::UserError {
                token,
                message: message.to_owned(),
            })
        }
    }

    fn synchronize(&self) -> Result<(), ParseError> {
        self.advance();

        loop {
            if self.is_at_end() {
                return Err(ParseError::EOF);
            }

            if self.previous().kind == TokenKind::Semicolon {
                return Ok(());
            }

            match self.peek()?.kind {
                TokenKind::Class
                | TokenKind::Fun
                | TokenKind::Var
                | TokenKind::For
                | TokenKind::If
                | TokenKind::While
                | TokenKind::Print
                | TokenKind::Return => return Ok(()),
                _ => self.advance(),
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

    fn advance(&self) -> &Token {
        if !self.is_at_end() {
            // safety: Relaxed ordering is fine since we are currently a single threaded parser.
            self.current.fetch_add(1, Ordering::Relaxed);
        }

        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.peek()
            .map(|v| v.kind == TokenKind::EOF)
            .unwrap_or(false)
    }

    fn peek(&self) -> Result<&Token, ParseError> {
        self.tokens
            .get(self.current.load(Ordering::Relaxed))
            .ok_or(ParseError::EOF)
    }

    fn previous(&self) -> &Token {
        debug_assert!(self.current.load(Ordering::Relaxed) > 0);

        self.tokens
            .get(self.current.load(Ordering::Relaxed) - 1)
            .expect("This should be infallibe")
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn it_works() {
        let input = "1 + 5";
        let mut scanner = crate::Scanner::new(input);
        let parser = Parser::new(scanner.scan_tokens());

        let result = parser.expression();

        assert_eq!(result.is_ok(), true);

        if let Expr::Binary(bin) = result.unwrap() {
            match *bin.left {
                Expr::Literal(val) => {
                    match val.kind {
                        TokenKind::Number(num) => assert_eq!(num.0, 1f64),
                        _ => panic!("Binary expression left was not a number"),
                    };
                }
                _ => panic!("Parsed binary expression did not contain a literal"),
            };

            assert_eq!(bin.operator.0.kind, TokenKind::Plus);

            match *bin.right {
                Expr::Literal(val) => {
                    match val.kind {
                        TokenKind::Number(num) => assert_eq!(num.0, 5f64),
                        _ => panic!("RHS of binary expression is wrong"),
                    };
                }
                _ => panic!("Parsed expression is wrong"),
            }
        } else {
            panic!("Parsed expression was not a Binary expression")
        }
    }
}
