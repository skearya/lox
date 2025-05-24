use std::collections::HashMap;

use crate::ast::{Expr, Function, Stmt};

#[derive(Debug)]
pub struct Resolver {
    errored: bool,
    // Indicates if a variable is declared (false) or defined (true)
    // We can then determine if a variable is read in its own initializer
    scopes: Vec<HashMap<String, bool>>,
    // Storing amount of Environment lookups each unique Expr::Variable, Expr::Assign should look up
    // The key of this map is a raw pointer which should be fine as every Expr is heap allocated and never mutated/moved
    // The raw pointer should never be dereferenced and should only be used for comparisons
    locals: HashMap<*const Expr, usize>,
    function: FunctionType,
    class: ClassType,
}

#[derive(Debug, Clone, Copy)]
enum FunctionType {
    None,
    Function,
    Initializer,
    Method,
}

#[derive(Debug, Clone, Copy)]
enum ClassType {
    None,
    Class,
}

impl Resolver {
    pub fn new() -> Self {
        Self {
            errored: false,
            scopes: Vec::new(),
            locals: HashMap::new(),
            function: FunctionType::None,
            class: ClassType::Class,
        }
    }

    pub fn resolve(mut self, stmts: &[Stmt]) -> Option<HashMap<*const Expr, usize>> {
        for stmt in stmts {
            self.stmt(stmt);
        }

        if self.errored {
            None
        } else {
            Some(self.locals)
        }
    }

    fn expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Assign(assign) => {
                self.expr(&assign.value);
                self.resolve_local(expr, &assign.name);
            }
            Expr::Binary(binary) => {
                self.expr(&binary.left);
                self.expr(&binary.right);
            }
            Expr::Call(call) => {
                self.expr(&call.callee);

                for arg in &call.args {
                    self.expr(arg);
                }
            }
            Expr::Get(get) => {
                self.expr(&get.object);
            }
            Expr::Grouping(grouping) => self.expr(&grouping.expr),
            Expr::Literal(_literal) => {}
            Expr::Logical(logical) => {
                self.expr(&logical.left);
                self.expr(&logical.right);
            }
            Expr::Set(set) => {
                self.expr(&set.object);
                self.expr(&set.value);
            }
            Expr::This => {
                if matches!(self.class, ClassType::None) {
                    self.errored = true;
                    // TODO: Better error message
                    eprintln!("Can't use 'this' outside of a class");
                    return;
                }

                self.resolve_local(expr, "this");
            }
            Expr::Unary(unary) => self.expr(&unary.right),
            Expr::Variable(var) => {
                if let Some(scope) = self.scopes.last() {
                    if let Some(defined) = scope.get(var) {
                        if !*defined {
                            self.errored = true;
                            // TODO: Better error message
                            eprintln!("Can't read local variable in its own initializer")
                        }
                    }
                }

                self.resolve_local(expr, var);
            }
        }
    }

    fn stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expr(expr) => self.expr(expr),
            Stmt::Class(class) => {
                let prev = self.class;
                self.class = ClassType::Class;

                self.scopes.push(HashMap::from([("this".to_owned(), true)]));

                self.declare(class.name.clone());
                self.define(class.name.clone());

                for method in &class.methods {
                    let declaration = if method.name == "init" {
                        FunctionType::Initializer
                    } else {
                        FunctionType::Method
                    };

                    self.resolve_function(method, declaration);
                }

                self.scopes.pop();
                self.class = prev;
            }
            Stmt::Function(function) => {
                self.declare(function.name.clone());
                self.define(function.name.clone());

                self.resolve_function(function, FunctionType::Function);
            }
            Stmt::Var(var) => {
                self.declare(var.name.clone());

                if let Some(initializer) = &var.initializer {
                    self.expr(initializer);
                }

                self.define(var.name.clone());
            }
            Stmt::Return(expr) => {
                if let FunctionType::None = self.function {
                    self.errored = true;
                    // TODO: Better error message
                    eprintln!("Can't return from top-level code");
                }

                if let Some(expr) = expr {
                    if matches!(self.function, FunctionType::Initializer) {
                        // TODO: Better error message
                        eprintln!("Can't return a value from an initializer");
                    }

                    self.expr(expr);
                }
            }
            Stmt::Print(expr) => self.expr(expr),
            Stmt::If(if_stmt) => {
                self.expr(&if_stmt.condition);
                self.stmt(&if_stmt.then_stmt);

                if let Some(else_stmt) = &if_stmt.else_stmt {
                    self.stmt(else_stmt);
                }
            }
            Stmt::While(while_stmt) => {
                self.expr(&while_stmt.condition);
                self.stmt(&while_stmt.body);
            }
            Stmt::Block(stmts) => {
                self.scopes.push(HashMap::new());

                for stmt in stmts {
                    self.stmt(stmt);
                }

                self.scopes.pop();
            }
        }
    }

    fn declare(&mut self, name: String) {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(&name) {
                self.errored = true;
                // TODO: Better error message
                eprintln!("Already a variable with this name in this scope");
            }

            scope.insert(name, false);
        }
    }

    fn define(&mut self, name: String) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, true);
        }
    }

    fn resolve_local(&mut self, expr: &Expr, name: &str) {
        for (i, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(name) {
                dbg!(expr as *const Expr, name, i);
                self.locals.insert(expr as *const Expr, i);
                return;
            }
        }
    }

    fn resolve_function(&mut self, function: &Function, kind: FunctionType) {
        let prev = self.function;
        self.function = kind;

        self.scopes.push(HashMap::new());

        for param in &function.args {
            self.define(param.clone());
        }

        for stmt in &function.body {
            self.stmt(stmt);
        }

        self.scopes.pop();

        self.function = prev;
    }
}
