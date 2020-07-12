use std::{collections::HashMap, fmt, sync::RwLock};

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
    Identifier,
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

impl TokenKind {
    pub fn is_number(&self) -> bool {
        match *self {
            TokenKind::Number(_) => true,
            _ => false,
        }
    }

    pub fn is_str(&self) -> bool {
        match *self {
            TokenKind::Str(_) => true,
            _ => false,
        }
    }

    pub fn is_primary(&self) -> bool {
        match *self {
            TokenKind::False
            | TokenKind::True
            | TokenKind::Nil
            | TokenKind::Number(_)
            | TokenKind::Str(_) => true,
            _ => false,
        }
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Debug)]
pub struct EthString(String);

impl fmt::Display for EthString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub struct EthNum(pub f64);

impl fmt::Display for EthNum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    range: (usize, usize),
    line: usize,
}

impl Token {
    pub fn new(kind: TokenKind, start: usize, end: usize, line: usize) -> Self {
        Self {
            kind,
            line,
            range: (start, end),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // write!(f, "line {}", self.line)?;
        match &self.kind {
            TokenKind::LeftParen => write!(f, " '('"),
            TokenKind::RightParen => write!(f, " ')'"),
            TokenKind::LeftBrace => write!(f, " '{{'"),
            TokenKind::RightBrace => write!(f, " '}}'"),
            TokenKind::Comma => write!(f, " ','"),
            TokenKind::Dot => write!(f, " '.'"),
            TokenKind::Minus => write!(f, " '-'"),
            TokenKind::Plus => write!(f, " '+'"),
            TokenKind::Semicolon => write!(f, " ';'"),
            TokenKind::Slash => write!(f, " '/'"),
            TokenKind::Star => write!(f, " '*'"),
            TokenKind::Bang => write!(f, " '!'"),
            TokenKind::BangEqual => write!(f, " '!='"),
            TokenKind::Equal => write!(f, " '='"),
            TokenKind::EqualEqual => write!(f, " '=='"),
            TokenKind::Greater => write!(f, " '>'"),
            TokenKind::GreaterEqual => write!(f, " '>="),
            TokenKind::Less => write!(f, " '<'"),
            TokenKind::LessEqual => write!(f, " '<="),
            TokenKind::Identifier => write!(f, " ident"),
            TokenKind::Str(v) => write!(f, " '{}'", v),
            TokenKind::Number(v) => write!(f, " '{}'", v),
            TokenKind::And => write!(f, " '&&'"),
            TokenKind::Class => write!(f, " 'class'"),
            TokenKind::Else => write!(f, " 'else'"),
            TokenKind::False => write!(f, " 'false'"),
            TokenKind::Fun => write!(f, " 'fun'"),
            TokenKind::For => write!(f, " 'for'"),
            TokenKind::If => write!(f, " 'if'"),
            TokenKind::Nil => write!(f, " 'nil'"),
            TokenKind::Or => write!(f, " 'or'"),
            TokenKind::Print => write!(f, " 'print'"),
            TokenKind::Return => write!(f, " 'return'"),
            TokenKind::Super => write!(f, " 'super'"),
            TokenKind::This => write!(f, " 'this'"),
            TokenKind::True => write!(f, " 'true'"),
            TokenKind::Var => write!(f, " 'var'"),
            TokenKind::While => write!(f, " 'while'"),
            TokenKind::EOF => write!(f, " 'EOF'"),
        }
    }
}

pub struct Scanner<'a> {
    source: &'a str,
    tokens: Vec<Token>,
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

    pub fn scan_tokens(&mut self) -> &[Token] {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.tokens.push(Token::new(
            TokenKind::EOF,
            self.source.len(),
            self.source.len(),
            self.line,
        ));

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
            Some('\n') => {
                self.line += 1;
            }
            val @ _ => {
                if val.map(|c| c.is_ascii_digit()) == Some(true) {
                    self.number();
                } else if val.map(|c| c.is_alphabetic() || c == '_') == Some(true) {
                    self.identifier();
                } else {
                    error(
                        self.line,
                        format!("Unexpected character: {:?}", val).as_str(),
                    );
                }
            }
        }
    }

    fn identifier(&mut self) {
        while let Some(true) = self.peek().map(|c| c.is_alphanumeric() || c == '_') {
            self.advance();
        }

        self.source
            .get(self.start..self.current)
            .and_then(|string| {
                // See if the identifier is a reserved keyword
                if let Some(val) = KEYWORDS.read().unwrap().get(string) {
                    self.add_token(val.clone());
                } else {
                    self.add_token(TokenKind::Identifier);
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
        let left;
        let right;

        if let TokenKind::Str(_) = kind {
            // Handling strings separately to trim the quotes.const
            left = self.start + 1;
            right = self.current - 1;
        } else {
            left = self.start;
            right = self.current;
        }
        self.tokens.push(Token::new(kind, left, right, self.line))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use pretty_assertions::assert_eq;

    #[test]
    fn it_works() {
        let text = "\"Hello\"";
        let expected = vec![
            Token::new(TokenKind::Str(EthString(String::from("Hello"))), 1, 6, 1),
            Token::new(TokenKind::EOF, 7, 7, 1),
        ];

        let mut scanner = Scanner::new(text);
        assert_eq!(scanner.scan_tokens(), &expected[..]);
    }

    #[test]
    fn identifier() {
        let text = "andy formless fo _ _123 _abc ab123";
        let expected = vec![
            Token::new(TokenKind::Identifier, 0, 4, 1),
            Token::new(TokenKind::Identifier, 5, 13, 1),
            Token::new(TokenKind::Identifier, 14, 16, 1),
            Token::new(TokenKind::Identifier, 17, 18, 1),
            Token::new(TokenKind::Identifier, 19, 23, 1),
            Token::new(TokenKind::Identifier, 24, 28, 1),
            Token::new(TokenKind::Identifier, 29, 34, 1),
            Token::new(TokenKind::EOF, 34, 34, 1),
        ];

        let mut scanner = Scanner::new(text);
        assert_eq!(scanner.scan_tokens(), &expected[..]);
    }

    #[test]
    fn longer_ident() {
        let text = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_";
        let expected = vec![
            Token::new(TokenKind::Identifier, 0, 63, 1),
            Token::new(TokenKind::EOF, 63, 63, 1),
        ];

        let mut scanner = Scanner::new(text);
        assert_eq!(scanner.scan_tokens(), &expected[..]);
    }

    #[test]
    fn keywords() {
        let text = "and class else false for fun if nil or return super this true var while";
        let expected = vec![
            Token::new(TokenKind::And, 0, 3, 1),
            Token::new(TokenKind::Class, 4, 9, 1),
            Token::new(TokenKind::Else, 10, 14, 1),
            Token::new(TokenKind::False, 15, 20, 1),
            Token::new(TokenKind::For, 21, 24, 1),
            Token::new(TokenKind::Fun, 25, 28, 1),
            Token::new(TokenKind::If, 29, 31, 1),
            Token::new(TokenKind::Nil, 32, 35, 1),
            Token::new(TokenKind::Or, 36, 38, 1),
            Token::new(TokenKind::Return, 39, 45, 1),
            Token::new(TokenKind::Super, 46, 51, 1),
            Token::new(TokenKind::This, 52, 56, 1),
            Token::new(TokenKind::True, 57, 61, 1),
            Token::new(TokenKind::Var, 62, 65, 1),
            Token::new(TokenKind::While, 66, 71, 1),
            Token::new(TokenKind::EOF, 71, 71, 1),
        ];

        let mut scanner = Scanner::new(text);
        assert_eq!(scanner.scan_tokens(), &expected[..]);
    }

    #[test]
    fn numbers() {
        let text = "123";
        let expected = vec![
            Token::new(TokenKind::Number(EthNum(123f64)), 0, 3, 1),
            Token::new(TokenKind::EOF, 3, 3, 1),
        ];

        let mut scanner = Scanner::new(text);
        assert_eq!(scanner.scan_tokens(), &expected[..]);

        let text = "123.456";
        let expected = vec![
            Token::new(TokenKind::Number(EthNum(123.456f64)), 0, 7, 1),
            Token::new(TokenKind::EOF, 7, 7, 1),
        ];

        let mut scanner = Scanner::new(text);
        assert_eq!(scanner.scan_tokens(), &expected[..]);

        let text = ".456";
        let expected = vec![
            Token::new(TokenKind::Dot, 0, 1, 1),
            Token::new(TokenKind::Number(EthNum(456f64)), 1, 4, 1),
            Token::new(TokenKind::EOF, 4, 4, 1),
        ];

        let mut scanner = Scanner::new(text);
        assert_eq!(scanner.scan_tokens(), &expected[..]);

        let text = "123.";
        let expected = vec![
            Token::new(TokenKind::Number(EthNum(123f64)), 0, 3, 1),
            Token::new(TokenKind::Dot, 3, 4, 1),
            Token::new(TokenKind::EOF, 4, 4, 1),
        ];

        let mut scanner = Scanner::new(text);
        assert_eq!(scanner.scan_tokens(), &expected[..]);
    }

    #[test]
    fn punctuators() {
        let text = "(){};,+-*!===<=>=!=<>/.";
        let expected = vec![
            Token::new(TokenKind::LeftParen, 0, 1, 1),
            Token::new(TokenKind::RightParen, 1, 2, 1),
            Token::new(TokenKind::LeftBrace, 2, 3, 1),
            Token::new(TokenKind::RightBrace, 3, 4, 1),
            Token::new(TokenKind::Semicolon, 4, 5, 1),
            Token::new(TokenKind::Comma, 5, 6, 1),
            Token::new(TokenKind::Plus, 6, 7, 1),
            Token::new(TokenKind::Minus, 7, 8, 1),
            Token::new(TokenKind::Star, 8, 9, 1),
            Token::new(TokenKind::BangEqual, 9, 11, 1),
            Token::new(TokenKind::EqualEqual, 11, 13, 1),
            Token::new(TokenKind::LessEqual, 13, 15, 1),
            Token::new(TokenKind::GreaterEqual, 15, 17, 1),
            Token::new(TokenKind::BangEqual, 17, 19, 1),
            Token::new(TokenKind::Less, 19, 20, 1),
            Token::new(TokenKind::Greater, 20, 21, 1),
            Token::new(TokenKind::Slash, 21, 22, 1),
            Token::new(TokenKind::Dot, 22, 23, 1),
            Token::new(TokenKind::EOF, 23, 23, 1),
        ];

        let mut scanner = Scanner::new(text);
        assert_eq!(scanner.scan_tokens(), &expected[..]);
    }

    #[test]
    fn whitespace() {
        let text = "space    tabs				newlines
        
        
        ";
        let expected = vec![
            Token::new(TokenKind::Identifier, 0, 5, 1),
            Token::new(TokenKind::Identifier, 9, 13, 1),
            Token::new(TokenKind::Identifier, 17, 25, 1),
            Token::new(TokenKind::EOF, 52, 52, 4),
        ];

        let mut scanner = Scanner::new(text);
        assert_eq!(scanner.scan_tokens(), &expected[..]);
    }
}
