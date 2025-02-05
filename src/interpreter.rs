use crate::{
    expr::{Binary, BinaryOp, Grouping, Unary, UnaryOp},
    token::Literal,
    visitor::Visitor,
};

#[derive(Debug)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    String(String),
}

#[derive(Debug)]
pub enum InterpreterError {
    OperandNotNumber,
    OperandsNotNumber,
}

pub struct Interpreter;

impl Interpreter {
    fn is_truthy(value: &Value) -> bool {
        match value {
            Value::Nil => false,
            Value::Bool(bool) => *bool,
            _ => true,
        }
    }

    fn is_equal(left: &Value, right: &Value) -> bool {
        match (left, right) {
            (Value::Nil, Value::Nil) => true,
            (Value::Bool(left), Value::Bool(right)) => left == right,
            (Value::Number(left), Value::Number(right)) => left == right,
            (Value::String(left), Value::String(right)) => left == right,
            _ => false,
        }
    }
}

impl Visitor for Interpreter {
    type Output = Result<Value, InterpreterError>;

    fn visit_binary(&mut self, binary: &Binary) -> Self::Output {
        let left = self.visit(&binary.left)?;
        let right = self.visit(&binary.right)?;

        let value = match (left, &binary.operator, right) {
            (left, BinaryOp::EqualEqual, right) => {
                Value::Bool(Interpreter::is_equal(&left, &right))
            }
            (left, BinaryOp::BangEqual, right) => {
                Value::Bool(!Interpreter::is_equal(&left, &right))
            }
            (Value::String(left), BinaryOp::Plus, Value::String(right)) => {
                Value::String(left + &right)
            }
            (Value::Number(left), operator, Value::Number(right)) => match operator {
                BinaryOp::Minus => Value::Number(left - right),
                BinaryOp::Plus => Value::Number(left + right),
                BinaryOp::Slash => Value::Number(left / right),
                BinaryOp::Star => Value::Number(left * right),
                BinaryOp::Greater => Value::Bool(left > right),
                BinaryOp::GreaterEqual => Value::Bool(left >= right),
                BinaryOp::Less => Value::Bool(left < right),
                BinaryOp::LessEqual => Value::Bool(left <= right),
                BinaryOp::EqualEqual | BinaryOp::BangEqual => unreachable!(),
            },
            _ => return Err(InterpreterError::OperandsNotNumber),
        };

        Ok(value)
    }

    fn visit_grouping(&mut self, grouping: &Grouping) -> Self::Output {
        self.visit(&grouping.expr)
    }

    fn visit_literal(&mut self, literal: &Literal) -> Self::Output {
        let value = match literal {
            Literal::Nil => Value::Nil,
            Literal::Bool(bool) => Value::Bool(*bool),
            Literal::String(str) => Value::String(str.clone()),
            Literal::Number(num) => Value::Number(*num),
        };

        Ok(value)
    }

    fn visit_unary(&mut self, unary: &Unary) -> Self::Output {
        let right = self.visit(&unary.right)?;

        let value = match (&unary.operator, right) {
            (UnaryOp::Minus, Value::Number(num)) => Value::Number(-num),
            (UnaryOp::Minus, _) => return Err(InterpreterError::OperandNotNumber),
            (UnaryOp::Bang, value) => Value::Bool(!Interpreter::is_truthy(&value)),
        };

        Ok(value)
    }
}
