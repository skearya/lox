use crate::token::{Literal, TokenKind};

#[derive(Debug)]
pub enum BinaryOp {
    Minus,
    Plus,
    Slash,
    Star,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    EqualEqual,
    BangEqual,
}

impl From<TokenKind> for BinaryOp {
    fn from(value: TokenKind) -> Self {
        match value {
            TokenKind::Minus => BinaryOp::Minus,
            TokenKind::Plus => BinaryOp::Plus,
            TokenKind::Slash => BinaryOp::Slash,
            TokenKind::Star => BinaryOp::Star,
            TokenKind::Greater => BinaryOp::Greater,
            TokenKind::GreaterEqual => BinaryOp::GreaterEqual,
            TokenKind::Less => BinaryOp::Less,
            TokenKind::LessEqual => BinaryOp::LessEqual,
            TokenKind::EqualEqual => BinaryOp::EqualEqual,
            TokenKind::BangEqual => BinaryOp::BangEqual,
            _ => panic!("value should've been a binary operator"),
        }
    }
}

#[derive(Debug)]
pub enum UnaryOp {
    Bang,
    Minus,
}

impl From<TokenKind> for UnaryOp {
    fn from(value: TokenKind) -> Self {
        match value {
            TokenKind::Bang => UnaryOp::Bang,
            TokenKind::Minus => UnaryOp::Minus,
            _ => panic!("value should've been a unary operator"),
        }
    }
}

#[derive(Debug)]
pub struct Binary {
    pub left: Expr,
    pub operator: BinaryOp,
    pub right: Expr,
}

#[derive(Debug)]
pub struct Grouping {
    pub expr: Expr,
}

#[derive(Debug)]
pub struct Unary {
    pub operator: UnaryOp,
    pub right: Expr,
}

impl Binary {
    pub fn new(left: Expr, operator: BinaryOp, right: Expr) -> Self {
        Self {
            left,
            operator,
            right,
        }
    }
}

impl Grouping {
    pub fn new(expr: Expr) -> Self {
        Self { expr }
    }
}

impl Unary {
    pub fn new(operator: UnaryOp, right: Expr) -> Self {
        Self { operator, right }
    }
}

#[derive(Debug)]
pub enum Expr {
    Binary(Box<Binary>),
    Grouping(Box<Grouping>),
    Literal(Literal),
    Unary(Box<Unary>),
    Variable(String),
}

#[derive(Debug)]
pub struct Var {
    name: String,
    initializer: Option<Expr>,
}

impl Var {
    pub fn new(name: String, initializer: Option<Expr>) -> Self {
        Self { name, initializer }
    }
}

pub enum Stmt {
    Expr(Expr),
    Print(Expr),
    Var(Var),
}
