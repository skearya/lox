use std::{
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
};

use crate::interpreter::{self, Callable, Environment, Interpreter, Value};

pub fn register_globals(environment: &Environment) {
    environment.define("clock".to_owned(), Value::Function(Rc::new(Clock)));
}

struct Clock;

impl Callable for Clock {
    fn arity(&self) -> u8 {
        0
    }

    fn call(
        &self,
        _interpreter: &mut Interpreter,
        _arguments: Vec<Value>,
    ) -> interpreter::Result<Value> {
        let time = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("time shouldn't go backwards")
            .as_secs_f64();

        Ok(Value::Number(time))
    }
}
