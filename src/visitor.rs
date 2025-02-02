use crate::{
    expr::{Binary, Expr, Grouping, Unary},
    token::Literal,
};

pub trait ExprVisitor: Sized {
    type Output;

    fn visit_binary(&mut self, binary: &Binary) -> Self::Output;
    fn visit_grouping(&mut self, grouping: &Grouping) -> Self::Output;
    fn visit_literal(&mut self, literal: &Literal) -> Self::Output;
    fn visit_unary(&mut self, unary: &Unary) -> Self::Output;
}

impl Expr {
    pub fn accept<V>(&self, visitor: &mut impl ExprVisitor<Output = V>) -> V {
        match self {
            Expr::Binary(binary) => visitor.visit_binary(binary),
            Expr::Grouping(grouping) => visitor.visit_grouping(grouping),
            Expr::Literal(literal) => visitor.visit_literal(literal),
            Expr::Unary(unary) => visitor.visit_unary(unary),
        }
    }
}
