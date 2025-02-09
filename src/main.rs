mod ast;
mod interpreter;
mod parser;
mod printer;
mod scanner;
mod token;
mod visitor;

use std::{
    collections::VecDeque,
    io::{self, Write},
};

use ast::Stmt;
use interpreter::Interpreter;
use parser::{ParseError, Parser};
use scanner::Scanner;
use token::Token;

fn main() {
    let scanner = Scanner::new(include_str!("test.lox"));
    let tokens: VecDeque<Token> = scanner.into_iter().collect();

    let parser = Parser::new(tokens);
    let stmts: Result<Vec<Stmt>, ParseError> = parser.into_iter().collect();

    match stmts {
        Ok(stmts) => {
            let mut interpreter = Interpreter;
            interpreter.interpret(&stmts);
        }
        Err(err) => eprintln!("parse error: {err:#?}"),
    }

    loop {
        let mut line = String::new();

        print!("> ");
        io::stdout().flush().expect("failed to flush stdout");

        io::stdin()
            .read_line(&mut line)
            .expect("failed to read line");

        let scanner = Scanner::new(&line);
        let tokens: VecDeque<Token> = scanner.into_iter().collect();

        let parser = Parser::new(tokens);
        let stmts: Result<Vec<Stmt>, ParseError> = parser.into_iter().collect();

        match stmts {
            Ok(stmts) => {
                let mut interpreter = Interpreter;
                interpreter.interpret(&stmts);
            }
            Err(err) => eprintln!("parse error: {err:#?}"),
        }
    }
}
