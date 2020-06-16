use std::{collections::HashMap, sync::RwLock};

use once_cell::{sync::Lazy, sync_lazy};

use crate::error::error;

static KEYWORDS: Lazy<RwLock<HashMap<&'static str, TokenKind>>> = sync_lazy! {
    let mut m = HashMap::new();
    m.insert("and", TokenKind::And);
    m.insert("class", TokenKind::Class);
    m.insert("else", TokenKind::Else);
    m.insert("false", TokenKind::False);
    m.insert("for", TokenKind::For);
    m.insert("fun", TokenKind::Fun);
    m.insert("if", TokenKind::If);
    m.insert("nil", TokenKind::Nil);
    m.insert("or", TokenKind::Or);
    m.insert("print", TokenKind::Print);
    m.insert("return", TokenKind::Return);
    m.insert("super", TokenKind::Super);
    m.insert("this", TokenKind::This);
    m.insert("true", TokenKind::True);
    m.insert("var", TokenKind::Var);
    m.insert("while", TokenKind::While);

    RwLock::new(m)
};

#[derive(Clone, PartialEq, Debug)]
pub enum TokenKind {
    // Single-character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals,
    Identifiers,
    Str(EthString),
    Number(EthNum),

    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    EOF,
}

#[derive(Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Debug)]
pub struct EthString(String);

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub struct EthNum(f64);

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'a> {
    kind: TokenKind,
    lexeme: &'a str,
    line: usize,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenKind, lexeme: &'a str, line: usize) -> Self {
        Self { kind, lexeme, line }
    }
}

pub struct Scanner<'a> {
    source: &'a str,
    tokens: Vec<Token<'a>>,
    current: usize,
    start: usize,
    line: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            tokens: Vec::new(),
            current: 0,
            start: 0,
            line: 1,
        }
    }

    pub fn scan_tokens(&mut self) -> &[Token<'a>] {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.tokens.push(Token::new(TokenKind::EOF, "", self.line));

        &self.tokens
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn scan_token(&mut self) {
        let c = self.advance();

        match c {
            Some('(') => self.add_token(TokenKind::LeftParen),
            Some(')') => self.add_token(TokenKind::RightParen),
            Some('{') => self.add_token(TokenKind::LeftBrace),
            Some('}') => self.add_token(TokenKind::RightBrace),
            Some(',') => self.add_token(TokenKind::Comma),
            Some('.') => self.add_token(TokenKind::Dot),
            Some('-') => self.add_token(TokenKind::Minus),
            Some('+') => self.add_token(TokenKind::Plus),
            Some(';') => self.add_token(TokenKind::Semicolon),
            Some('*') => self.add_token(TokenKind::Star),
            Some('!') => {
                if self.match_char('=') {
                    self.add_token(TokenKind::BangEqual)
                } else {
                    self.add_token(TokenKind::Bang)
                }
            }
            Some('=') => {
                if self.match_char('=') {
                    self.add_token(TokenKind::EqualEqual)
                } else {
                    self.add_token(TokenKind::Equal)
                }
            }
            Some('<') => {
                if self.match_char('=') {
                    self.add_token(TokenKind::LessEqual)
                } else {
                    self.add_token(TokenKind::Less)
                }
            }
            Some('>') => {
                if self.match_char('=') {
                    self.add_token(TokenKind::GreaterEqual)
                } else {
                    self.add_token(TokenKind::Greater)
                }
            }
            Some('/') => {
                if self.match_char('/') {
                    while self.peek() != Some('\n') && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(TokenKind::Slash);
                }
            }
            Some('"') => self.string(),
            // Ignore whitespace
            Some(' ') | Some('\r') | Some('\t') => {}
            val @ _ => {
                if val.map(|c| c.is_ascii_digit()) == Some(true) {
                    self.number();
                } else if val.map(|c| c.is_alphabetic() || c == '_') == Some(true) {
                    self.identifier();
                } else {
                    error(self.line, "Unexpected character.")
                }
            }
        }
    }

    fn identifier(&mut self) {
        while let Some(true) = self.peek().map(|c| c.is_alphanumeric()) {
            self.advance();
        }

        self.source
            .get(self.start..self.current)
            .and_then(|string| {
                // See if the identifier is a reserved keyword
                if let Some(val) = KEYWORDS.read().unwrap().get(string) {
                    self.add_token(val.clone());
                } else {
                    self.add_token(TokenKind::Identifiers);
                }

                Some(())
            });
    }

    fn number(&mut self) {
        while let Some(true) = self.peek().map(|c| c.is_ascii_digit()) {
            self.advance();
        }

        // Look for the fractional part
        if self.peek() == Some('.') && self.peek_next().map(|c| c.is_ascii_digit()) == Some(true) {
            // Consume the "."
            self.advance();

            while let Some(true) = self.peek().map(|c| c.is_ascii_digit()) {
                self.advance();
            }
        }

        let value = self
            .source
            .get(self.start..self.current)
            .unwrap()
            .parse()
            .expect("Unable to parse f64");
        self.add_token(TokenKind::Number(EthNum(value)));
    }

    fn peek_next(&self) -> Option<char> {
        if self.current + 1 >= self.source.len() {
            return Some('\0');
        }

        self.source.chars().nth(self.current + 1)
    }

    fn string(&mut self) {
        while self.peek() != Some('"') && !self.is_at_end() {
            if self.peek() == Some('\n') {
                self.line += 1;
            }
            self.advance();
        }

        // Unterminated string.
        if self.is_at_end() {
            error(self.line, "Unterminated string");
        }

        // The closing ".
        self.advance();

        // Trim the surrounding quotes
        let value = self
            .source
            .get(self.start + 1..self.current - 1)
            .unwrap()
            .to_owned();
        self.add_token(TokenKind::Str(EthString(value)));
    }

    fn peek(&self) -> Option<char> {
        if self.is_at_end() {
            return Some('\0');
        }
        self.source.chars().nth(self.current)
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }

        if self.source.chars().nth(self.current) != Some(expected) {
            return false;
        }

        self.current += 1;
        true
    }

    fn advance(&mut self) -> Option<char> {
        self.current += 1;
        self.source.chars().nth(self.current - 1)
    }

    fn add_token(&mut self, kind: TokenKind) {
        let text = self
            .source
            .get(self.start..self.current)
            .expect("Unable to get substr");
        self.tokens.push(Token::new(kind, text, self.line))
    }
}
