use std::{collections::HashMap, mem};

use crate::{
    ast::{
        Assign, Binary, BinaryOp, Expr, Grouping, If, Logical, LogicalOp, Stmt, Unary, UnaryOp,
        Var, While,
    },
    token::Literal,
    visitor::{ExprVisitor, StmtVisitor},
};

#[derive(Debug, Clone)]
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
    UndefinedVariable,
}

type Result<T> = core::result::Result<T, InterpreterError>;

pub struct Interpreter {
    environment: Environment,
}

#[derive(Debug, Clone, Default)]
struct Environment {
    enclosing: Option<Box<Environment>>,
    values: HashMap<String, Value>,
}

impl Environment {
    fn new() -> Self {
        Self {
            enclosing: None,
            values: HashMap::new(),
        }
    }

    fn enclosing(enclosing: Box<Environment>) -> Self {
        Self {
            enclosing: Some(enclosing),
            values: HashMap::new(),
        }
    }

    fn get(&mut self, name: &str) -> Result<Value> {
        if let Some(value) = self.values.get(name).cloned() {
            Ok(value)
        } else {
            match self.enclosing.as_mut() {
                Some(enclosing) => enclosing.get(name),
                None => Err(InterpreterError::UndefinedVariable),
            }
        }
    }

    fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    fn assign(&mut self, name: String, value: Value) -> Result<()> {
        if self.values.contains_key(&name) {
            self.values.insert(name, value);

            Ok(())
        } else {
            match self.enclosing.as_mut() {
                Some(enclosing) => enclosing.assign(name, value),
                None => Err(InterpreterError::UndefinedVariable),
            }
        }
    }
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            environment: Environment::new(),
        }
    }

    pub fn interpret(&mut self, stmts: &[Stmt]) {
        for stmt in stmts {
            if let Err(err) = self.visit_stmt(stmt) {
                eprintln!("runtime error: {err:#?}");
            }
        }
    }

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

impl StmtVisitor for Interpreter {
    type Output = Result<()>;

    fn visit_expr_stmt(&mut self, expr: &Expr) -> Self::Output {
        self.visit_expr(expr)?;

        Ok(())
    }

    fn visit_var_stmt(&mut self, var: &Var) -> Self::Output {
        let value = match &var.initializer {
            Some(initializer) => self.visit_expr(initializer)?,
            None => Value::Nil,
        };

        self.environment.define(var.name.clone(), value);

        Ok(())
    }

    fn visit_print_stmt(&mut self, expr: &Expr) -> Self::Output {
        dbg!(self.visit_expr(expr)?);

        Ok(())
    }

    fn visit_if_stmt(&mut self, stmt: &If) -> Self::Output {
        let condition = self.visit_expr(&stmt.condition)?;

        if Interpreter::is_truthy(&condition) {
            self.visit_stmt(&stmt.then_stmt)?;
        } else if let Some(else_stmt) = &stmt.else_stmt {
            self.visit_stmt(else_stmt)?;
        }

        Ok(())
    }

    fn visit_while_stmt(&mut self, while_stmt: &While) -> Self::Output {
        while Interpreter::is_truthy(&self.visit_expr(&while_stmt.condition)?) {
            self.visit_stmt(&while_stmt.body)?;
        }

        Ok(())
    }

    fn visit_block_stmt(&mut self, block: &[Stmt]) -> Self::Output {
        self.environment = Environment::enclosing(Box::new(mem::take(&mut self.environment)));

        for stmt in block {
            self.visit_stmt(stmt)?;
        }

        self.environment = mem::take(
            self.environment
                .enclosing
                .as_mut()
                .expect("should have parent environment"),
        );

        Ok(())
    }
}

impl ExprVisitor for Interpreter {
    type Output = Result<Value>;

    fn visit_binary(&mut self, binary: &Binary) -> Self::Output {
        let left = self.visit_expr(&binary.left)?;
        let right = self.visit_expr(&binary.right)?;

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

    fn visit_assign(&mut self, assign: &Assign) -> Self::Output {
        let value = self.visit_expr(&assign.value)?;

        self.environment
            .assign(assign.name.clone(), value.clone())?;

        Ok(value)
    }

    fn visit_grouping(&mut self, grouping: &Grouping) -> Self::Output {
        self.visit_expr(&grouping.expr)
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
        let right = self.visit_expr(&unary.right)?;

        let value = match (&unary.operator, right) {
            (UnaryOp::Minus, Value::Number(num)) => Value::Number(-num),
            (UnaryOp::Minus, _) => return Err(InterpreterError::OperandNotNumber),
            (UnaryOp::Bang, value) => Value::Bool(!Interpreter::is_truthy(&value)),
        };

        Ok(value)
    }

    fn visit_logical(&mut self, logical: &Logical) -> Self::Output {
        let left = self.visit_expr(&logical.left)?;
        let truthy = Interpreter::is_truthy(&left);

        match logical.operator {
            LogicalOp::Or if truthy => return Ok(left),
            LogicalOp::And if !truthy => return Ok(left),
            _ => {}
        };

        self.visit_expr(&logical.right)
    }

    fn visit_var(&mut self, var: &str) -> Self::Output {
        self.environment.get(var)
    }
}
