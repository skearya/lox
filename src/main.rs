mod expr;
mod parser;
mod printer;
mod scanner;
mod token;
mod visitor;

use std::collections::VecDeque;

use parser::Parser;
use printer::Printer;
use scanner::Scanner;
use token::Token;

#[allow(non_snake_case)]
fn P<T>(item: T) -> Box<T> {
    Box::new(item)
}

fn main() {
    let scanner = Scanner::new(include_str!("test.lox"));
    let tokens: VecDeque<Token> = scanner.into_iter().collect();

    let mut parser = Parser::new(tokens);
    let expr = dbg!(parser.expression());
    dbg!(expr.unwrap().accept(&mut Printer));

    // if let Some(filename) = env::args().nth(1) {
    //     let file = fs::read_to_string(filename).unwrap();
    // } else {
    //     for line in io::stdin().lines() {}
    // }
}
