use std::{collections::HashMap, mem};

use crate::{
    ast::{BinaryOp, Expr, LogicalOp, Stmt, UnaryOp},
    token::Literal,
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
            if let Err(err) = self.stmt(stmt) {
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

    fn expr(&mut self, expr: &Expr) -> Result<Value> {
        let value = match expr {
            Expr::Binary(binary) => {
                let left = self.expr(&binary.left)?;
                let right = self.expr(&binary.right)?;

                match (left, &binary.operator, right) {
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
                }
            }
            Expr::Assign(assign) => {
                let value = self.expr(&assign.value)?;

                self.environment
                    .assign(assign.name.clone(), value.clone())?;

                value
            }
            Expr::Grouping(grouping) => self.expr(&grouping.expr)?,
            Expr::Literal(literal) => match literal {
                Literal::Nil => Value::Nil,
                Literal::Bool(bool) => Value::Bool(*bool),
                Literal::String(str) => Value::String(str.clone()),
                Literal::Number(num) => Value::Number(*num),
            },
            Expr::Unary(unary) => {
                let right = self.expr(&unary.right)?;

                match (&unary.operator, right) {
                    (UnaryOp::Minus, Value::Number(num)) => Value::Number(-num),
                    (UnaryOp::Minus, _) => return Err(InterpreterError::OperandNotNumber),
                    (UnaryOp::Bang, value) => Value::Bool(!Interpreter::is_truthy(&value)),
                }
            }
            Expr::Logical(logical) => {
                let left = self.expr(&logical.left)?;
                let truthy = Interpreter::is_truthy(&left);

                match logical.operator {
                    LogicalOp::Or if truthy => return Ok(left),
                    LogicalOp::And if !truthy => return Ok(left),
                    _ => {}
                };

                self.expr(&logical.right)?
            }
            Expr::Variable(var) => self.environment.get(var)?,
        };

        Ok(value)
    }

    fn stmt(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Expr(expr) => {
                self.expr(expr)?;
            }
            Stmt::Var(var) => {
                let value = match &var.initializer {
                    Some(initializer) => self.expr(initializer)?,
                    None => Value::Nil,
                };

                self.environment.define(var.name.clone(), value);
            }
            Stmt::Print(expr) => {
                println!("{:?}", self.expr(expr)?);
            }
            Stmt::If(if_stmt) => {
                let condition = self.expr(&if_stmt.condition)?;

                if Interpreter::is_truthy(&condition) {
                    self.stmt(&if_stmt.then_stmt)?;
                } else if let Some(else_stmt) = &if_stmt.else_stmt {
                    self.stmt(else_stmt)?;
                }
            }
            Stmt::While(while_stmt) => {
                while Interpreter::is_truthy(&self.expr(&while_stmt.condition)?) {
                    self.stmt(&while_stmt.body)?;
                }
            }
            Stmt::Block(block) => {
                self.environment =
                    Environment::enclosing(Box::new(mem::take(&mut self.environment)));

                for stmt in block {
                    self.stmt(stmt)?;
                }

                self.environment = mem::take(
                    self.environment
                        .enclosing
                        .as_mut()
                        .expect("should have parent environment"),
                );
            }
        };

        Ok(())
    }
}
