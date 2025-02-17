use crate::{
    ast::{Assign, Binary, BinaryOp, Expr, Grouping, Logical, LogicalOp, Unary, UnaryOp},
    token::Literal,
    visitor::ExprVisitor,
};

pub struct Printer;

impl Printer {
    fn parenthesize<const N: usize>(&mut self, name: &str, exprs: [&Expr; N]) -> String {
        let mut output = String::new();

        output.push('(');
        output.push_str(name);

        for expr in exprs {
            output.push(' ');
            output.push_str(&expr.accept(self));
        }

        output.push(')');

        output
    }
}

impl ExprVisitor for Printer {
    type Output = String;

    fn visit_binary(&mut self, binary: &Binary) -> Self::Output {
        let operator = match binary.operator {
            BinaryOp::Minus => "-",
            BinaryOp::Plus => "+",
            BinaryOp::Slash => "/",
            BinaryOp::Star => "*",
            BinaryOp::Greater => ">",
            BinaryOp::GreaterEqual => ">=",
            BinaryOp::Less => "<",
            BinaryOp::LessEqual => "<=",
            BinaryOp::EqualEqual => "==",
            BinaryOp::BangEqual => "!=",
        };

        self.parenthesize(operator, [&binary.left, &binary.right])
    }

    fn visit_assign(&mut self, assign: &Assign) -> Self::Output {
        format!("{} = {:#?}", assign.name, assign.value)
    }

    fn visit_grouping(&mut self, grouping: &Grouping) -> Self::Output {
        self.parenthesize("group", [&grouping.expr])
    }

    fn visit_literal(&mut self, literal: &Literal) -> Self::Output {
        match literal {
            Literal::Nil => "nil".to_owned(),
            Literal::Bool(bool) => bool.to_string(),
            Literal::String(data) => data.to_owned(),
            Literal::Number(data) => data.to_string(),
        }
    }

    fn visit_unary(&mut self, unary: &Unary) -> Self::Output {
        let operator = match unary.operator {
            UnaryOp::Bang => "!",
            UnaryOp::Minus => "-",
        };

        self.parenthesize(operator, [&unary.right])
    }

    fn visit_logical(&mut self, logical: &Logical) -> Self::Output {
        let operator = match logical.operator {
            LogicalOp::Or => "or",
            LogicalOp::And => "and",
        };

        self.parenthesize(operator, [&logical.left, &logical.right])
    }

    fn visit_var(&mut self, var: &str) -> Self::Output {
        var.to_owned()
    }
}
