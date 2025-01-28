mod scanner;
mod token;

use std::{env, fs};

use scanner::Scanner;

fn main() {
    let mut scanner = Scanner::new(include_str!("test.lox"));
    println!("{:#?}", scanner.tokens())

    // if let Some(filename) = env::args().nth(1) {
    //     let file = fs::read_to_string(filename).unwrap();
    // } else {
    //     for line in io::stdin().lines() {}
    // }
}

enum Error {}

fn run(source: &str) -> Result<(), Error> {
    Ok(())
}
