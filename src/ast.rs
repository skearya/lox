use crate::token::{Literal, TokenKind};

#[derive(Debug, Clone)]
pub enum Expr {
    Assign(Box<Assign>),
    Binary(Box<Binary>),
    Call(Box<Call>),
    Grouping(Box<Grouping>),
    Literal(Literal),
    Unary(Box<Unary>),
    Logical(Box<Logical>),
    Variable(String),
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(Expr),
    Function(Function),
    Var(Var),
    Return(Option<Expr>),
    Print(Expr),
    If(Box<If>),
    While(Box<While>),
    Block(Vec<Stmt>),
}

#[derive(Debug, Clone)]
pub struct Assign {
    pub name: String,
    pub value: Expr,
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub left: Expr,
    pub operator: BinaryOp,
    pub right: Expr,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub callee: Expr,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct Grouping {
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub struct Unary {
    pub operator: UnaryOp,
    pub right: Expr,
}

#[derive(Debug, Clone)]
pub struct Logical {
    pub left: Expr,
    pub operator: LogicalOp,
    pub right: Expr,
}

#[derive(Debug, Clone)]
pub struct Var {
    pub name: String,
    pub initializer: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub args: Vec<String>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct If {
    pub condition: Expr,
    pub then_stmt: Stmt,
    pub else_stmt: Option<Stmt>,
}

#[derive(Debug, Clone)]
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

impl Call {
    pub fn new(callee: Expr, args: Vec<Expr>) -> Self {
        Self { callee, args }
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

impl Function {
    pub fn new(name: String, args: Vec<String>, body: Vec<Stmt>) -> Self {
        Self { name, args, body }
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Bang,
    Minus,
}

#[derive(Debug, Clone)]
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
