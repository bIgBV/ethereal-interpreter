use std::{
    env,
    io::{self, stdin, BufRead, Write},
};

mod common;
mod error;
mod interpreter;
mod parser;
mod printer;
mod scanner;
mod state;

use interpreter::Interpreter;
use parser::Parser;
use scanner::Scanner;

fn main() {
    let args: Vec<String> = env::args().collect();

    if dbg!(dbg!(args.len()) > 1) {
        println!("Usage: jlox [script]");
    } else if args.len() == 2 {
        match run(&args[1]) {
            Ok(_) => (),
            Err(e) => panic!("{}", e),
        };
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
    let mut scanner = Scanner::new(path.as_ref());
    let parser = Parser::new(scanner.scan_tokens());
    let interpreter = Interpreter::new();

    let (stmts, errs) = parser.parse();

    if errs.len() > 0 {
        let length = errs.len();
        for err in errs {
            println!("{}", err);
        }
        println!("Execution failed due to {} errors.", length);
    } else {
        interpreter.interpret(&stmts);
    }

    Ok(())
}
