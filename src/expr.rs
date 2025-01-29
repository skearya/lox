use crate::token::Token;

struct Binary<'a> {
    left: Expr<'a>,
    operator: Token<'a>,
    right: Expr<'a>,
}

struct Grouping<'a> {
    expr: Expr<'a>,
}

struct Literal<'a> {
    value: Token<'a>,
}

struct Unary<'a> {
    operator: Token<'a>,
    right: Expr<'a>,
}

enum Expr<'a> {
    Binary(Box<Binary<'a>>),
    Grouping(Box<Grouping<'a>>),
    Literal(Box<Literal<'a>>),
    Unary(Box<Unary<'a>>),
}
