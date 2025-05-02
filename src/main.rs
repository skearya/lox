use ariadne::{Color, ColorGenerator, Fmt, Label, Report, ReportKind, Source};
use ast::Stmt;
use parser::Parser;
use scanner::Scanner;

mod ast;
mod error;
mod parser;
mod scanner;
mod token;

fn main() {
    let source = include_str!("sample.lox");

    let scanner = Scanner::new(source);
    let parser = Parser::new(source, scanner);

    dbg!(parser.collect::<Vec<Stmt>>());
}
