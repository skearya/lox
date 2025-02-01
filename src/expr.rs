use crate::token::{Token, TokenKind};

pub enum Operator {
    Minus,
    Plus,
    Slash,
    Star,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

impl Operator {
    pub fn from_token(token: &Token) -> Option<Operator> {
        match token.kind {
            TokenKind::Minus => Some(Operator::Minus),
            TokenKind::Plus => Some(Operator::Plus),
            TokenKind::Slash => Some(Operator::Slash),
            TokenKind::Star => Some(Operator::Star),
            TokenKind::Bang => Some(Operator::Bang),
            TokenKind::BangEqual => Some(Operator::BangEqual),
            TokenKind::Equal => Some(Operator::Equal),
            TokenKind::EqualEqual => Some(Operator::EqualEqual),
            TokenKind::Greater => Some(Operator::Greater),
            TokenKind::GreaterEqual => Some(Operator::GreaterEqual),
            TokenKind::Less => Some(Operator::Less),
            TokenKind::LessEqual => Some(Operator::LessEqual),
            _ => None,
        }
    }

    pub fn to_str(&self) -> &'static str {
        match self {
            Operator::Minus => "-",
            Operator::Plus => "+",
            Operator::Star => "*",
            Operator::BangEqual => "!=",
            Operator::Bang => "!",
            Operator::EqualEqual => "==",
            Operator::Equal => "=",
            Operator::LessEqual => "<=",
            Operator::Less => "<",
            Operator::GreaterEqual => ">=",
            Operator::Greater => ">",
            Operator::Slash => "/",
        }
    }
}

pub struct Binary {
    pub left: Expr,
    pub operator: Operator,
    pub right: Expr,
}

pub struct Grouping {
    pub expr: Expr,
}

pub enum Literal {
    Keyword(String),
    String(String),
    Number(f64),
}

pub struct Unary {
    pub operator: Operator,
    pub right: Expr,
}

impl Binary {
    pub fn new(left: Expr, operator: Operator, right: Expr) -> Self {
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
    pub fn new(operator: Operator, right: Expr) -> Self {
        Self { operator, right }
    }
}

pub enum Expr {
    Binary(Box<Binary>),
    Grouping(Box<Grouping>),
    Literal(Literal),
    Unary(Box<Unary>),
}
