use yaplwrs;

fn main() {
    match yaplwrs::parse("fn foo() -> bar {}") {
        Ok(n) => eprintln!("tree: {}", n),
        Err(e) => eprintln!("{:?}", e),
    }
}
