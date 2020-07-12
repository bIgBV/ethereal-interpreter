use std::{
    env,
    io::{self, stdin, BufRead, Write},
};

pub mod error;
mod interpreter;
pub mod parser;
pub mod printer;
pub mod scanner;

use interpreter::Interpreter;
use parser::Parser;
use scanner::Scanner;

fn main() {
    let args: Vec<String> = env::args().collect();

    if dbg!(dbg!(args.len()) > 1) {
        println!("Usage: jlox [script]");
    } else if args.len() == 2 {
        run(&args[1]);
    } else {
        run_prompt();
    }
    println!("Hello, world!");
}

fn run_prompt() {
    let stdin = stdin();
    let mut reader = stdin.lock();
    let mut buffer = String::new();

    println!("Running ethereal-interpreter 0.0.1");

    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        reader
            .read_line(&mut buffer)
            .expect("Unable to read to string");
        match run(&buffer) {
            Ok(_) => (),
            Err(e) => println!("{}", e),
        };
        buffer.clear();
    }
}

fn run<T: AsRef<str>>(path: T) -> Result<(), Box<dyn std::error::Error>> {
    dbg!(path.as_ref());
    let mut scanner = Scanner::new(path.as_ref());
    let parser = Parser::new(scanner.scan_tokens());
    let mut interpreter = Interpreter;

    match parser.expression() {
        Ok(expr) => println!("{}", interpreter.interpret(&expr)?),
        Err(e) => println!("Error: {}", e),
    }

    Ok(())
}
