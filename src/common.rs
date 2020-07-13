use std::fmt;

pub trait Visitor {
    type Out;

    fn visit_expr(&self, expr: &Expr) -> Self::Out;
    fn visit_binary(&self, expr: &Expr) -> Self::Out;
    fn visit_literal(&self, expr: &Expr) -> Self::Out;
    fn visit_unary(&self, expr: &Expr) -> Self::Out;
    fn visit_group(&self, expr: &Expr) -> Self::Out;
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Token),
    Group(Box<Expr>),
    Binary(Binary),
    Unary(Unary),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Unary {
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binary {
    pub left: Box<Expr>,
    pub operator: Operator,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Operator(pub Token);

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

    pub fn is_literal(&self) -> bool {
        match *self {
            TokenKind::Number(_) | TokenKind::Str(_) | TokenKind::False | TokenKind::True => true,
            _ => false,
        }
    }
}

pub fn is_truthy(kind: &TokenKind) -> bool {
    match *kind {
        TokenKind::Nil | TokenKind::False => false,
        _ => true,
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Debug)]
pub struct EthString(pub String);

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
