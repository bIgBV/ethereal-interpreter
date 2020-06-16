pub fn error(line: usize, message: &str) {
    report(line, "", message);
}

fn report(line: usize, location: &str, message: &str) {
    println!("[line {}] Error {}: {}", line, location, message)
}
