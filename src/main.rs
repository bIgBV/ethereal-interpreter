use std::{
    env, fs,
    io::{self, stdin, BufRead, Write},
};

mod common;
mod environment;
mod error;
mod interpreter;
mod parser;
mod printer;
mod scanner;

use interpreter::Interpreter;
use parser::Parser;
use scanner::Scanner;

struct State {
    interpreter: Interpreter,
    source: String,
}

impl State {
    pub fn new() -> Self {
        State {
            interpreter: Interpreter::new(),
            source: String::new(),
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    let state = State::new();

    if args.len() == 1 {
        run_prompt(state)?;
    } else if args.len() == 2 {
        run_file(&args[1], state)?;
    } else {
        println!("Usage: eth [script]");
    }
    Ok(())
}

fn run_file(path: &str, mut state: State) -> Result<(), Box<dyn std::error::Error>> {
    let content = fs::read_to_string(path)?;
    run(content, &mut state.interpreter)?;

    Ok(())
}

fn run_prompt(mut state: State) -> Result<(), Box<dyn std::error::Error>> {
    let stdin = stdin();
    let mut reader = stdin.lock();
    let mut buffer = state.source;

    println!("Running ethereal-interpreter 0.0.1");

    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        reader
            .read_line(&mut buffer)
            .expect("Unable to read to string");
        run(&buffer, &mut state.interpreter)?;
        buffer.clear();
    }
}

fn run<T: AsRef<str>>(
    path: T,
    interpreter: &mut Interpreter,
) -> Result<(), Box<dyn std::error::Error>> {
    let mut scanner = Scanner::new(path.as_ref());
    let parser = Parser::new(scanner.scan_tokens());

    let (stmts, errs) = parser.parse();

    if errs.len() > 0 {
        let length = errs.len();
        for err in errs {
            println!("{}", err);
        }
        println!("Execution failed due to {} errors.", length);
    } else {
        interpreter.interpret(&stmts)?;
    }

    Ok(())
}
