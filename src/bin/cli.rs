extern crate yaplwrs;

fn main() {
    match yaplwrs::parse("") {
        Ok(_) => {}
        Err(e) => eprintln!("{}", e),
    }
}
