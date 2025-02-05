use crate::{
    expr::{Binary, Expr, Grouping, Unary},
    token::Literal,
    visitor::Visitor,
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

impl Visitor for Printer {
    type Output = String;

    fn visit_binary(&mut self, binary: &Binary) -> Self::Output {
        self.parenthesize(binary.operator.to_str(), [&binary.left, &binary.right])
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
        self.parenthesize(unary.operator.to_str(), [&unary.right])
    }
}
