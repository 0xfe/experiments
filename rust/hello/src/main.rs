mod print;
mod strings;
mod types;
mod vars;
mod tuples;

fn main() {
    println!("Hello, world!");
    println!("Foo bar, foo foo!");

    print::run();
    vars::run();
    types::run();
    strings::run();
    tuples::run();
}