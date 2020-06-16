use std::{
    env,
    io::{self, stdin, BufRead, Write},
};

pub mod error;
mod scanner;

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
        run(&buffer);
        buffer.clear();
    }
}

fn run<T: AsRef<str>>(path: T) {
    dbg!(path.as_ref());
    let mut scanner = Scanner::new(path.as_ref());

    for token in scanner.scan_tokens() {
        println!("{:?}", token);
    }
}
