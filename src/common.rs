use std::{
    fmt,
    hash::{Hash, Hasher},
    ops::{Deref, DerefMut},
};

pub trait ExprVisitor<'us, 'source: 'us> {
    type Out;

    fn visit_expr(&'us self, expr: &'source Expr) -> Self::Out;
    fn visit_binary(&'us self, expr: &'source Expr) -> Self::Out;
    fn visit_literal(&'us self, expr: &'source Expr) -> Self::Out;
    fn visit_unary(&'us self, expr: &'source Expr) -> Self::Out;
    fn visit_group(&'us self, expr: &'source Expr) -> Self::Out;
    fn visit_var(&'us self, expr: &'source Expr) -> Self::Out;
}

pub trait StmtVisitor<'us, 'source: 'us> {
    type Out;

    fn visit_stmt(&'us self, stmt: &'source Stmt) -> Self::Out;
    fn visit_print(&'us self, stmt: &'source Stmt) -> Self::Out;
    fn visit_expr_stmt(&'us self, stmt: &'source Stmt) -> Self::Out;
    fn visit_var_stmt(&'us self, stmt: &'source Stmt) -> Self::Out;
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Token),
    Group(Box<Expr>),
    Binary(Binary),
    Unary(Unary),
    Variable(Token),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Expr(Expr),
    Print(Expr),
    Var(Variable),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub name: Token,
    pub init: Option<Expr>,
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

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub range: (usize, usize),
    line: usize,
}

impl PartialEq for Token {
    fn eq(&self, other: &Token) -> bool {
        let (start1, end1) = self.range;
        let (start2, end2) = other.range;

        // Tokens are equal if they were parsed from the same piece of source.
        start1 == start2 && end1 == end2
    }
}

impl Eq for Token {}

impl Hash for Token {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.line.hash(state);

        // Two tokens will always have different ranges.
        let (start, end) = self.range;
        start.hash(state);
        end.hash(state);
    }
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

#[derive(Debug)]
pub enum Value {
    Num(EthNum),
    Str(EthString),
    Bool(bool),
    Nil,
}

#[derive(Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Debug)]
pub struct EthString(pub String);

impl fmt::Display for EthString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Deref for EthString {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for EthString {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub struct EthNum(pub f64);

impl fmt::Display for EthNum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Deref for EthNum {
    type Target = f64;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for EthNum {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
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
