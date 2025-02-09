use crate::{
    ast::{Binary, Expr, Grouping, Stmt, Unary, Var},
    token::Literal,
};

pub trait StmtVisitor: Sized {
    type Output;

    fn visit(&mut self, stmt: &Stmt) -> Self::Output {
        stmt.accept(self)
    }

    fn visit_expr_stmt(&mut self, expr: &Expr) -> Self::Output;
    fn visit_print_stmt(&mut self, expr: &Expr) -> Self::Output;
    fn visit_var_stmt(&mut self, var: &Var) -> Self::Output;
}

impl Stmt {
    pub fn accept<V>(&self, visitor: &mut impl StmtVisitor<Output = V>) -> V {
        match self {
            Stmt::Expr(expr) => visitor.visit_expr_stmt(expr),
            Stmt::Print(expr) => visitor.visit_print_stmt(expr),
            Stmt::Var(var) => visitor.visit_var_stmt(var),
        }
    }
}

pub trait ExprVisitor: Sized {
    type Output;

    fn visit(&mut self, expr: &Expr) -> Self::Output {
        expr.accept(self)
    }

    fn visit_binary(&mut self, binary: &Binary) -> Self::Output;
    fn visit_grouping(&mut self, grouping: &Grouping) -> Self::Output;
    fn visit_literal(&mut self, literal: &Literal) -> Self::Output;
    fn visit_unary(&mut self, unary: &Unary) -> Self::Output;
    fn visit_var(&mut self, var: &str) -> Self::Output;
}

impl Expr {
    pub fn accept<V>(&self, visitor: &mut impl ExprVisitor<Output = V>) -> V {
        match self {
            Expr::Binary(binary) => visitor.visit_binary(binary),
            Expr::Grouping(grouping) => visitor.visit_grouping(grouping),
            Expr::Literal(literal) => visitor.visit_literal(literal),
            Expr::Unary(unary) => visitor.visit_unary(unary),
            Expr::Variable(var) => visitor.visit_var(var),
        }
    }
}
