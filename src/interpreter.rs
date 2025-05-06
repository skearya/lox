use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::Debug,
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
};

use crate::{
    ast::{BinaryOp, Expr, Function, LogicalOp, Stmt, UnaryOp},
    token::Literal,
};

#[derive(Debug)]
pub enum InterpreterError {
    Return(Value),

    OperandNotNumber,
    OperandsNotNumber,
    UndefinedVariable,
    UncallableCallee,
    ArgumentArityMismatch,
}

type Result<T> = core::result::Result<T, InterpreterError>;

#[derive(Debug)]
pub struct Interpreter {
    environment: Rc<Environment>,
}

#[derive(Debug, Clone, Default)]
struct Environment {
    enclosing: Option<Rc<Environment>>,
    values: RefCell<HashMap<String, Value>>,
}

#[derive(Clone)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    String(String),
    Function(Rc<dyn Callable>),
}

pub trait Callable {
    fn arity(&self) -> u8;
    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<Value>) -> Result<Value>;
}

impl Callable for Function {
    fn arity(&self) -> u8 {
        self.args.len() as u8
    }

    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<Value>) -> Result<Value> {
        let environment = Rc::new(Environment::enclosing(Rc::clone(&interpreter.environment)));

        for (name, value) in self.args.iter().zip(arguments) {
            environment.define(name.clone(), value);
        }

        interpreter.block(&self.body, environment)?;

        Ok(Value::Nil)
    }
}

impl Interpreter {
    pub fn new() -> Self {
        struct Clock;

        impl Callable for Clock {
            fn arity(&self) -> u8 {
                0
            }

            fn call(
                &self,
                _interpreter: &mut Interpreter,
                _arguments: Vec<Value>,
            ) -> Result<Value> {
                let time = SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .expect("time shouldn't go backwards")
                    .as_secs_f64();

                Ok(Value::Number(time))
            }
        }

        let environment = Rc::new(Environment::new());

        environment.define("clock".to_owned(), Value::Function(Rc::new(Clock)));

        Self { environment }
    }

    pub fn interpret(mut self, stmts: &[Stmt]) {
        for stmt in stmts {
            if let Err(err) = self.stmt(stmt) {
                eprintln!("Runtime error: {err:#?}");
                return;
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
            Expr::Call(call) => {
                let callee = self.expr(&call.callee)?;

                if let Value::Function(function) = callee {
                    if function.arity() != call.args.len() as u8 {
                        return Err(InterpreterError::ArgumentArityMismatch);
                    }

                    let arguments = call
                        .args
                        .iter()
                        .map(|arg| self.expr(arg))
                        .collect::<Result<Vec<Value>>>()?;

                    match function.call(self, arguments) {
                        Ok(value) => value,
                        Err(InterpreterError::Return(value)) => value,
                        Err(other) => return Err(other),
                    }
                } else {
                    return Err(InterpreterError::UncallableCallee);
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
                    LogicalOp::Or if truthy => left,
                    LogicalOp::And if !truthy => left,
                    _ => self.expr(&logical.right)?,
                }
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
            Stmt::Function(function) => {
                self.environment.define(
                    function.name.clone(),
                    Value::Function(Rc::new(function.clone())),
                );
            }
            Stmt::Var(var) => {
                let value = match &var.initializer {
                    Some(initializer) => self.expr(initializer)?,
                    None => Value::Nil,
                };

                self.environment.define(var.name.clone(), value);
            }
            Stmt::Return(expr) => {
                let value = match expr {
                    Some(expr) => self.expr(expr)?,
                    None => Value::Nil,
                };

                return Err(InterpreterError::Return(value));
            }
            Stmt::Print(expr) => match self.expr(expr)? {
                Value::Nil => println!("nil"),
                Value::Bool(bool) => println!("{bool}"),
                Value::Number(num) => println!("{num}"),
                Value::String(str) => println!("{str}"),
                Value::Function(_callable) => println!("<fn>"),
            },
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
            Stmt::Block(stmts) => {
                self.block(
                    stmts,
                    Rc::new(Environment::enclosing(Rc::clone(&self.environment))),
                )?;
            }
        }

        Ok(())
    }

    fn block(&mut self, stmts: &[Stmt], environment: Rc<Environment>) -> Result<()> {
        let current = Rc::clone(&self.environment);

        self.environment = environment;

        for stmt in stmts {
            if let Err(err) = self.stmt(stmt) {
                self.environment = current;

                return Err(err);
            }
        }

        self.environment = current;

        Ok(())
    }
}

impl Environment {
    fn new() -> Self {
        Self {
            enclosing: None,
            values: RefCell::new(HashMap::new()),
        }
    }

    fn enclosing(enclosing: Rc<Environment>) -> Self {
        Self {
            enclosing: Some(enclosing),
            values: RefCell::new(HashMap::new()),
        }
    }

    fn get(&self, name: &str) -> Result<Value> {
        if let Some(value) = self.values.borrow_mut().get(name).cloned() {
            Ok(value)
        } else {
            match &self.enclosing {
                Some(enclosing) => enclosing.get(name),
                None => Err(InterpreterError::UndefinedVariable),
            }
        }
    }

    fn define(&self, name: String, value: Value) {
        self.values.borrow_mut().insert(name, value);
    }

    fn assign(&self, name: String, value: Value) -> Result<()> {
        if self.values.borrow().contains_key(&name) {
            self.values.borrow_mut().insert(name, value);

            Ok(())
        } else {
            match &self.enclosing {
                Some(enclosing) => enclosing.assign(name, value),
                None => Err(InterpreterError::UndefinedVariable),
            }
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nil => write!(f, "Nil"),
            Self::Bool(bool) => f.debug_tuple("Bool").field(bool).finish(),
            Self::Number(num) => f.debug_tuple("Number").field(num).finish(),
            Self::String(str) => f.debug_tuple("String").field(str).finish(),
            Self::Function(_function) => write!(f, "<fn>"),
        }
    }
}
