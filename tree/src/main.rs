use std::{env, fs};

use interpreter::Interpreter;
use parser::Parser;
use resolver::Resolver;
use scanner::Scanner;

mod ast;
mod error;
mod global;
mod interpreter;
mod parser;
mod resolver;
mod scanner;
mod token;

fn main() {
    let args = env::args().collect::<Vec<String>>();

    let filename = &args.get(1).expect("expected filename");
    let source = fs::read_to_string(filename).expect("error reading file");

    let scanner = Scanner::new(&source);

    let Some(stmts) = Parser::new(&source, scanner).parse() else {
        return;
    };

    let Some(locals) = Resolver::new().resolve(&stmts) else {
        return;
    };

    Interpreter::new(locals).interpret(&stmts);
}
