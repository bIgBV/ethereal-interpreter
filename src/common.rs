use std::{
    fmt,
    hash::{Hash, Hasher},
    ops::{Deref, DerefMut},
};

pub trait ExprVisitor<O> {
    fn visit_expr(&self, expr: &Expr) -> O {
        match expr {
            Expr::Literal(_) => self.visit_literal(expr),
            Expr::Binary(_) => self.visit_binary(expr),
            Expr::Group(_) => self.visit_group(expr),
            Expr::Unary(_) => self.visit_unary(expr),
            Expr::Variable(_) => self.visit_var(expr),
            Expr::Assign(_) => self.visit_assign(expr),
            Expr::Logical(_) => self.visit_logical(expr),
        }
    }

    fn visit_binary(&self, expr: &Expr) -> O;
    fn visit_literal(&self, expr: &Expr) -> O;
    fn visit_unary(&self, expr: &Expr) -> O;
    fn visit_group(&self, expr: &Expr) -> O;
    fn visit_var(&self, expr: &Expr) -> O;
    fn visit_assign(&self, expr: &Expr) -> O;
    fn visit_logical(&self, expr: &Expr) -> O;
}

pub trait StmtVisitor<O> {
    fn visit_stmt(&self, stmt: &Stmt) -> O {
        match *stmt {
            Stmt::Expr(_) => self.visit_expr_stmt(stmt),
            Stmt::Print(_) => self.visit_print(stmt),
            Stmt::Var(_) => self.visit_var_stmt(stmt),
            Stmt::Block(_) => self.visit_block_stmt(stmt),
            Stmt::If(_) => self.visit_if_stmt(stmt),
            Stmt::While(_) => self.visit_while_stmt(stmt),
        }
    }

    fn visit_print(&self, stmt: &Stmt) -> O;
    fn visit_expr_stmt(&self, stmt: &Stmt) -> O;
    fn visit_var_stmt(&self, stmt: &Stmt) -> O;
    fn visit_block_stmt(&self, stmt: &Stmt) -> O;
    fn visit_if_stmt(&self, stmt: &Stmt) -> O;
    fn visit_while_stmt(&self, stmt: &Stmt) -> O;
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Token),
    Group(Box<Expr>),
    Binary(Binary),
    Unary(Unary),
    Variable(Token),
    Assign(Assign),
    Logical(Logical),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Expr(Expr),
    Print(Expr),
    Var(Variable),
    Block(Vec<Stmt>),
    If(If),
    While(While),
}

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    pub cond: Expr,
    pub then: Box<Stmt>,
    pub unless: Box<Option<Stmt>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct While {
    pub cond: Expr,
    pub body: Box<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub name: Token,
    pub init: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assign {
    pub name: Token,
    pub value: Box<Expr>,
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
pub struct Logical {
    pub left: Box<Expr>,
    pub operator: Operator,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Operator(pub Token);

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    line: usize,
}

impl PartialEq for Token {
    fn eq(&self, other: &Token) -> bool {
        self.lexeme == other.lexeme
    }
}

impl Eq for Token {}

impl Hash for Token {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.line.hash(state);

        self.lexeme.hash(state)
    }
}

impl Token {
    pub fn new(kind: TokenKind, lexeme: &str, line: usize) -> Self {
        Self {
            kind,
            line,
            lexeme: String::from(lexeme),
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
