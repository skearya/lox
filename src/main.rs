mod expr;
mod interpreter;
mod parser;
mod printer;
mod scanner;
mod token;
mod visitor;

use std::collections::VecDeque;

use interpreter::Interpreter;
use parser::Parser;
use printer::Printer;
use scanner::Scanner;
use token::Token;

fn main() {
    let scanner = Scanner::new(include_str!("test.lox"));
    let tokens: VecDeque<Token> = scanner.into_iter().collect();

    let mut parser = Parser::new(tokens);
    let expr = parser.expression().unwrap();
    dbg!(expr.accept(&mut Printer));

    let _ = dbg!(expr.accept(&mut Interpreter));

    // if let Some(filename) = env::args().nth(1) {
    //     let file = fs::read_to_string(filename).unwrap();
    // } else {
    //     for line in io::stdin().lines() {}
    // }
}
