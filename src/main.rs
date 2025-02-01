mod expr;
mod parser;
mod printer;
mod scanner;
mod token;
mod visitor;

use expr::{Binary, Expr, Literal, Operator};
use printer::Printer;
use scanner::Scanner;
use token::{Token, TokenKind};

#[allow(non_snake_case)]
fn P<T>(item: T) -> Box<T> {
    Box::new(item)
}

fn main() {
    let mut scanner = Scanner::new(include_str!("test.lox"));
    dbg!(scanner.tokens());

    let expr = Expr::Binary(P(Binary::new(
        Expr::Literal(Literal::Number(100.0)),
        Operator::Plus,
        Expr::Literal(Literal::Number(100.0)),
    )));

    dbg!(expr.accept(&mut Printer));

    // if let Some(filename) = env::args().nth(1) {
    //     let file = fs::read_to_string(filename).unwrap();
    // } else {
    //     for line in io::stdin().lines() {}
    // }
}
