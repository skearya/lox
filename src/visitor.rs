use crate::{
    ast::{Assign, Binary, Expr, Grouping, If, Logical, Stmt, Unary, Var, While},
    token::Literal,
};

pub trait StmtVisitor: Sized {
    type Output;

    fn visit_stmt(&mut self, stmt: &Stmt) -> Self::Output {
        stmt.accept(self)
    }

    fn visit_expr_stmt(&mut self, expr: &Expr) -> Self::Output;
    fn visit_var_stmt(&mut self, var: &Var) -> Self::Output;
    fn visit_print_stmt(&mut self, expr: &Expr) -> Self::Output;
    fn visit_if_stmt(&mut self, if_stmt: &If) -> Self::Output;
    fn visit_while_stmt(&mut self, while_stmt: &While) -> Self::Output;
    fn visit_block_stmt(&mut self, block: &[Stmt]) -> Self::Output;
}

impl Stmt {
    pub fn accept<V>(&self, visitor: &mut impl StmtVisitor<Output = V>) -> V {
        match self {
            Stmt::Expr(expr) => visitor.visit_expr_stmt(expr),
            Stmt::Var(var) => visitor.visit_var_stmt(var),
            Stmt::Print(expr) => visitor.visit_print_stmt(expr),
            Stmt::If(if_stmt) => visitor.visit_if_stmt(if_stmt),
            Stmt::While(while_stmt) => visitor.visit_while_stmt(while_stmt),
            Stmt::Block(block) => visitor.visit_block_stmt(block),
        }
    }
}

pub trait ExprVisitor: Sized {
    type Output;

    fn visit_expr(&mut self, expr: &Expr) -> Self::Output {
        expr.accept(self)
    }

    fn visit_binary(&mut self, binary: &Binary) -> Self::Output;
    fn visit_assign(&mut self, assign: &Assign) -> Self::Output;
    fn visit_grouping(&mut self, grouping: &Grouping) -> Self::Output;
    fn visit_literal(&mut self, literal: &Literal) -> Self::Output;
    fn visit_unary(&mut self, unary: &Unary) -> Self::Output;
    fn visit_logical(&mut self, logical: &Logical) -> Self::Output;
    fn visit_var(&mut self, var: &str) -> Self::Output;
}

impl Expr {
    pub fn accept<V>(&self, visitor: &mut impl ExprVisitor<Output = V>) -> V {
        match self {
            Expr::Assign(assign) => visitor.visit_assign(assign),
            Expr::Binary(binary) => visitor.visit_binary(binary),
            Expr::Grouping(grouping) => visitor.visit_grouping(grouping),
            Expr::Literal(literal) => visitor.visit_literal(literal),
            Expr::Unary(unary) => visitor.visit_unary(unary),
            Expr::Logical(logical) => visitor.visit_logical(logical),
            Expr::Variable(var) => visitor.visit_var(var),
        }
    }
}
