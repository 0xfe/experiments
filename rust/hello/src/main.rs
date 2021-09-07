mod print;
mod strings;
mod types;
mod vars;
mod tuples;
mod arrays;
mod functions;
mod structs;
mod args;

fn main() {
    println!("Hello, world!");
    println!("Foo bar, foo foo!");

    print::run();
    vars::run();
    types::run();
    strings::run();
    tuples::run();
    arrays:: run();
    functions::run();
    structs::run();
    args::run();
}
