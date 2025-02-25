use crate::{
    ast::{BinaryOp, Expr, LogicalOp, UnaryOp},
    token::Literal,
};

pub fn print(expr: &Expr) -> String {
    match expr {
        Expr::Assign(assign) => {
            format!("{} = {:#?}", assign.name, assign.value)
        }
        Expr::Binary(binary) => {
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

            parenthesize(operator, [&binary.left, &binary.right])
        }
        Expr::Grouping(grouping) => parenthesize("group", [&grouping.expr]),
        Expr::Literal(literal) => match literal {
            Literal::Nil => "nil".to_owned(),
            Literal::Bool(bool) => bool.to_string(),
            Literal::String(data) => data.to_owned(),
            Literal::Number(data) => data.to_string(),
        },
        Expr::Unary(unary) => {
            let operator = match unary.operator {
                UnaryOp::Bang => "!",
                UnaryOp::Minus => "-",
            };

            parenthesize(operator, [&unary.right])
        }
        Expr::Logical(logical) => {
            let operator = match logical.operator {
                LogicalOp::Or => "or",
                LogicalOp::And => "and",
            };

            parenthesize(operator, [&logical.left, &logical.right])
        }
        Expr::Variable(name) => name.to_owned(),
    }
}

fn parenthesize<const N: usize>(name: &str, exprs: [&Expr; N]) -> String {
    let mut output = String::new();

    output.push('(');
    output.push_str(name);

    for expr in exprs {
        output.push(' ');
        output.push_str(&print(expr));
    }

    output.push(')');

    output
}
