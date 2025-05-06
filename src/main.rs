use interpreter::Interpreter;
use parser::Parser;
use scanner::Scanner;

mod ast;
mod error;
mod interpreter;
mod parser;
mod scanner;
mod token;

fn main() {
    let source = include_str!("sample.lox");

    let scanner = Scanner::new(source);
    let parser = Parser::new(source, scanner);

    if let Some(stmts) = parser.parse() {
        Interpreter::new().interpret(&stmts);
    }
}
