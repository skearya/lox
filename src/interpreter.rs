use std::{cell::RefCell, collections::HashMap, fmt::Debug, rc::Rc};

use crate::{
    ast::{BinaryOp, Expr, Function, LogicalOp, Stmt, UnaryOp},
    global::register_globals,
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
    ExpectedInstanceInGet,
    UndefinedProperty,
}

pub type Result<T> = core::result::Result<T, InterpreterError>;

#[derive(Debug)]
pub struct Interpreter {
    // Resolved variable lookup depth, more info in `resolver.rs`
    locals: HashMap<*const Expr, usize>,
    globals: Rc<Environment>,
    environment: Rc<Environment>,
}

#[derive(Debug, Clone, Default)]
pub struct Environment {
    enclosing: Option<Rc<Environment>>,
    values: RefCell<HashMap<String, Value>>,
}

#[derive(Clone)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    String(String),
    Class(Rc<LoxClass>),
    Instance(Rc<LoxInstance>),
    Function(Rc<dyn Callable>),
}

pub struct LoxClass {
    name: String,
    methods: HashMap<String, LoxFunction>,
}

pub struct LoxInstance {
    class: Rc<LoxClass>,
    fields: RefCell<HashMap<String, Value>>,
}

#[derive(Debug, Clone)]
struct LoxFunction {
    declaration: Function,
    closure: Rc<Environment>,
    initializer: bool,
}

pub trait Callable {
    fn arity(&self) -> u8;
    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<Value>) -> Result<Value>;
}

impl Interpreter {
    pub fn new(locals: HashMap<*const Expr, usize>) -> Self {
        let globals = Rc::new(Environment::new());

        register_globals(&globals);

        Self {
            locals,
            environment: Rc::clone(&globals),
            globals,
        }
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
            Expr::Assign(assign) => {
                let value = self.expr(&assign.value)?;

                let distance = self.locals.get(&(expr as *const Expr));

                match distance {
                    Some(distance) => {
                        self.environment
                            .assign_at(&assign.name, value.clone(), *distance)?
                    }
                    None => self.globals.assign(&assign.name, value.clone())?,
                }

                value
            }
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

                let mut call = |callable: &dyn Callable| -> Result<Value> {
                    if callable.arity() != call.args.len() as u8 {
                        return Err(InterpreterError::ArgumentArityMismatch);
                    }

                    let arguments = call
                        .args
                        .iter()
                        .map(|arg| self.expr(arg))
                        .collect::<Result<Vec<Value>>>()?;

                    match callable.call(self, arguments) {
                        Ok(value) => Ok(value),
                        Err(InterpreterError::Return(value)) => Ok(value),
                        Err(other) => return Err(other),
                    }
                };

                match callee {
                    Value::Class(class) => call(&class)?,
                    Value::Function(function) => call(function.as_ref())?,
                    _ => return Err(InterpreterError::UncallableCallee),
                }
            }
            Expr::Get(get) => {
                let object = self.expr(&get.object)?;

                if let Value::Instance(instance) = object {
                    match instance.get(&get.name) {
                        Some(value) => value,
                        None => return Err(InterpreterError::UndefinedProperty),
                    }
                } else {
                    return Err(InterpreterError::ExpectedInstanceInGet);
                }
            }
            Expr::Grouping(grouping) => self.expr(&grouping.expr)?,
            Expr::Literal(literal) => match literal {
                Literal::Nil => Value::Nil,
                Literal::Bool(bool) => Value::Bool(*bool),
                Literal::String(str) => Value::String(str.clone()),
                Literal::Number(num) => Value::Number(*num),
            },
            Expr::Logical(logical) => {
                let left = self.expr(&logical.left)?;
                let truthy = Interpreter::is_truthy(&left);

                match logical.operator {
                    LogicalOp::Or if truthy => left,
                    LogicalOp::And if !truthy => left,
                    _ => self.expr(&logical.right)?,
                }
            }
            Expr::Set(set) => {
                let object = self.expr(&set.object)?;

                if let Value::Instance(instance) = object {
                    let value = self.expr(&set.value)?;
                    instance.set(set.name.clone(), value.clone());

                    value
                } else {
                    return Err(InterpreterError::ExpectedInstanceInGet);
                }
            }
            Expr::This => self.lookup("expr", expr)?,
            Expr::Unary(unary) => {
                let right = self.expr(&unary.right)?;

                match (&unary.operator, right) {
                    (UnaryOp::Minus, Value::Number(num)) => Value::Number(-num),
                    (UnaryOp::Minus, _) => return Err(InterpreterError::OperandNotNumber),
                    (UnaryOp::Bang, value) => Value::Bool(!Interpreter::is_truthy(&value)),
                }
            }
            Expr::Variable(var) => self.lookup(&var, expr)?,
        };

        Ok(value)
    }

    fn stmt(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Expr(expr) => {
                self.expr(expr)?;
            }
            Stmt::Class(class) => {
                self.environment.define(class.name.clone(), Value::Nil);

                let mut methods = HashMap::new();

                for method in &class.methods {
                    methods.insert(
                        method.name.clone(),
                        LoxFunction::new(
                            method.clone(),
                            Rc::clone(&self.environment),
                            method.name == "init",
                        ),
                    );
                }

                self.environment.assign(
                    &class.name,
                    Value::Class(Rc::new(LoxClass::new(class.name.clone(), methods))),
                )?;
            }
            Stmt::Function(function) => {
                self.environment.define(
                    function.name.clone(),
                    Value::Function(Rc::new(LoxFunction::new(
                        function.clone(),
                        Rc::clone(&self.environment),
                        false,
                    ))),
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
                Value::Class(class) => println!("{}", class.name),
                Value::Function(_callable) => println!("<fn>"),
                Value::Instance(instance) => println!("{} instance", instance.class.name),
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
        let original = Rc::clone(&self.environment);

        self.environment = environment;

        for stmt in stmts {
            if let Err(err) = self.stmt(stmt) {
                self.environment = original;

                return Err(err);
            }
        }

        self.environment = original;

        Ok(())
    }

    pub fn lookup(&self, name: &str, expr: &Expr) -> Result<Value> {
        dbg!(name, expr as *const Expr);
        let distance = self.locals.get(&(expr as *const Expr));

        let value = match distance {
            Some(distance) => self.environment.get_at(name, *distance)?,
            None => self.globals.get(name)?,
        };

        Ok(value)
    }
}

impl Environment {
    pub fn new() -> Self {
        Self {
            enclosing: None,
            values: RefCell::new(HashMap::new()),
        }
    }

    pub fn enclosing(enclosing: Rc<Environment>) -> Self {
        Self {
            enclosing: Some(enclosing),
            values: RefCell::new(HashMap::new()),
        }
    }

    pub fn get(&self, name: &str) -> Result<Value> {
        match self.values.borrow().get(name).cloned() {
            Some(value) => Ok(value),
            None => Err(InterpreterError::UndefinedVariable),
        }
    }

    pub fn get_at(&self, name: &str, distance: usize) -> Result<Value> {
        self.ancestor(distance).get(name)
    }

    pub fn define(&self, name: String, value: Value) {
        self.values.borrow_mut().insert(name, value);
    }

    pub fn assign(&self, name: &str, value: Value) -> Result<()> {
        match self.values.borrow_mut().get_mut(name) {
            Some(var) => {
                *var = value;
                Ok(())
            }
            None => Err(InterpreterError::UndefinedVariable),
        }
    }

    pub fn assign_at(&self, name: &str, value: Value, distance: usize) -> Result<()> {
        self.ancestor(distance).assign(name, value)
    }

    fn ancestor(&self, distance: usize) -> Rc<Environment> {
        let mut environment = Rc::new(self.clone());

        for _ in 0..distance {
            environment = self
                .enclosing
                .clone()
                .expect("variable lookup distance should've been right");
        }

        environment
    }
}

impl LoxClass {
    fn new(name: String, methods: HashMap<String, LoxFunction>) -> Self {
        Self { name, methods }
    }

    fn method(&self, name: &str) -> Option<LoxFunction> {
        self.methods.get(name).cloned()
    }
}

impl LoxInstance {
    pub fn new(class: Rc<LoxClass>) -> Self {
        Self {
            class,
            fields: RefCell::new(HashMap::new()),
        }
    }

    fn get(self: Rc<Self>, name: &str) -> Option<Value> {
        if let Some(value) = self.fields.borrow_mut().get(name).cloned() {
            Some(value)
        } else if let Some(function) = self.class.method(name) {
            Some(Value::Function(Rc::new(function.bind(Rc::clone(&self)))))
        } else {
            None
        }
    }

    fn set(&self, name: String, value: Value) {
        self.fields.borrow_mut().insert(name, value);
    }
}

impl LoxFunction {
    fn new(declaration: Function, closure: Rc<Environment>, initializer: bool) -> Self {
        Self {
            declaration,
            closure,
            initializer,
        }
    }

    fn bind(&self, instance: Rc<LoxInstance>) -> LoxFunction {
        let environment = Rc::new(Environment::enclosing(Rc::clone(&self.closure)));

        environment.define("this".to_owned(), Value::Instance(instance));

        Self {
            declaration: self.declaration.clone(),
            closure: environment,
            initializer: self.initializer,
        }
    }
}

impl Callable for Rc<LoxClass> {
    fn arity(&self) -> u8 {
        match self.methods.get("init") {
            Some(initializer) => initializer.arity(),
            None => 0,
        }
    }

    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<Value>) -> Result<Value> {
        let instance = Rc::new(LoxInstance::new(Rc::clone(&self)));

        if let Some(initalizer) = self.method("init") {
            initalizer
                .bind(Rc::clone(&instance))
                .call(interpreter, arguments)?;
        }

        Ok(Value::Instance(instance))
    }
}

impl Callable for LoxFunction {
    fn arity(&self) -> u8 {
        self.declaration.args.len() as u8
    }

    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<Value>) -> Result<Value> {
        let environment = Rc::new(Environment::enclosing(Rc::clone(&self.closure)));

        for (name, value) in self.declaration.args.iter().zip(arguments) {
            environment.define(name.clone(), value);
        }

        interpreter.block(&self.declaration.body, environment)?;

        if self.initializer {
            return Ok(self
                .closure
                .get("this")
                .expect("expected parent environment contain 'this' binding"));
        }

        Ok(Value::Nil)
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nil => write!(f, "Nil"),
            Self::Bool(arg0) => f.debug_tuple("Bool").field(arg0).finish(),
            Self::Number(arg0) => f.debug_tuple("Number").field(arg0).finish(),
            Self::String(arg0) => f.debug_tuple("String").field(arg0).finish(),
            Self::Class(_arg0) => write!(f, "Class"),
            Self::Instance(_arg0) => write!(f, "Instance"),
            Self::Function(_arg0) => write!(f, "Function"),
        }
    }
}
