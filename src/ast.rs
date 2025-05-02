use crate::token::{Literal, TokenKind};

#[derive(Debug)]
pub enum Expr {
    Assign(Box<Assign>),
    Binary(Box<Binary>),
    Grouping(Box<Grouping>),
    Literal(Literal),
    Unary(Box<Unary>),
    Logical(Box<Logical>),
    Variable(String),
}

#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),
    Var(Var),
    Print(Expr),
    If(Box<If>),
    While(Box<While>),
    Block(Vec<Stmt>),
}

#[derive(Debug)]
pub struct Assign {
    pub name: String,
    pub value: Expr,
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

#[derive(Debug)]
pub struct Logical {
    pub left: Expr,
    pub operator: LogicalOp,
    pub right: Expr,
}

#[derive(Debug)]
pub struct Var {
    pub name: String,
    pub initializer: Option<Expr>,
}

#[derive(Debug)]
pub struct If {
    pub condition: Expr,
    pub then_stmt: Stmt,
    pub else_stmt: Option<Stmt>,
}

#[derive(Debug)]
pub struct While {
    pub condition: Expr,
    pub body: Stmt,
}

impl Assign {
    pub fn new(name: String, value: Expr) -> Self {
        Self { name, value }
    }
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

impl Logical {
    pub fn new(left: Expr, operator: LogicalOp, right: Expr) -> Self {
        Self {
            left,
            operator,
            right,
        }
    }
}

impl Var {
    pub fn new(name: String, initializer: Option<Expr>) -> Self {
        Self { name, initializer }
    }
}

impl If {
    pub fn new(condition: Expr, then_stmt: Stmt, else_stmt: Option<Stmt>) -> Self {
        Self {
            condition,
            then_stmt,
            else_stmt,
        }
    }
}

impl While {
    pub fn new(condition: Expr, body: Stmt) -> Self {
        Self { condition, body }
    }
}

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

#[derive(Debug)]
pub enum UnaryOp {
    Bang,
    Minus,
}

#[derive(Debug)]
pub enum LogicalOp {
    Or,
    And,
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
            _ => unreachable!("value should've been a binary operator"),
        }
    }
}

impl From<TokenKind> for UnaryOp {
    fn from(value: TokenKind) -> Self {
        match value {
            TokenKind::Bang => UnaryOp::Bang,
            TokenKind::Minus => UnaryOp::Minus,
            _ => unreachable!("value should've been a unary operator"),
        }
    }
}

impl From<TokenKind> for LogicalOp {
    fn from(value: TokenKind) -> Self {
        match value {
            TokenKind::Or => LogicalOp::Or,
            TokenKind::And => LogicalOp::And,
            _ => unreachable!("value should've been a logial operator"),
        }
    }
}
