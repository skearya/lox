use crate::{
    expr::{Binary, Expr, Grouping, Literal, Unary},
    visitor::ExprVisitor,
};

pub struct Printer;

impl ExprVisitor for Printer {
    type Output = String;

    fn visit_binary(&mut self, binary: &Binary) -> Self::Output {
        parenthesize(
            self,
            binary.operator.to_str(),
            [&binary.left, &binary.right],
        )
    }

    fn visit_grouping(&mut self, grouping: &Grouping) -> Self::Output {
        parenthesize(self, "group", [&grouping.expr])
    }

    fn visit_literal(&mut self, literal: &Literal) -> Self::Output {
        match literal {
            Literal::Keyword(data) | Literal::String(data) => data.clone(),
            Literal::Number(data) => data.to_string(),
        }
    }

    fn visit_unary(&mut self, unary: &Unary) -> Self::Output {
        parenthesize(self, unary.operator.to_str(), [&unary.right])
    }
}

fn parenthesize<const N: usize>(visitor: &mut Printer, name: &str, exprs: [&Expr; N]) -> String {
    let mut output = String::new();

    output.push('(');
    output.push_str(name);

    for expr in exprs {
        output.push(' ');
        output.push_str(&expr.accept(visitor));
    }

    output.push(')');

    output
}
