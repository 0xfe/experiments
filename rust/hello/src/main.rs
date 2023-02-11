mod args;
mod arrays;
mod functions;
mod generics;
mod iterators;
mod json;
mod lifetimes;
mod pointers;
mod print;
mod server;
mod strings;
mod structs;
mod threadpool;
mod threads;
mod traits;
mod tuples;
mod types;
mod vars;

use std::env;

fn process_cmdline() {
    // env::args() returns an iterator. collect() pulls the elements
    // into a vector.
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        println!("Usage: {} (print|traits|threads|...)", args[0]);
        std::process::exit(-1);
    }

    match args[1].as_str() {
        "print" => print::run(),
        "strings" => strings::run(),
        "types" => types::run(),
        "vars" => vars::run(),
        "tuples" => tuples::run(),
        "arrays" => arrays::run(),
        "functions" => functions::run(),
        "structs" => structs::run(),
        "args" => args::run(),
        "lifetimes" => lifetimes::run(),
        "traits" => traits::run(),
        "generics" => generics::run(),
        "threads" => threads::run(),
        "iterators" => iterators::run(),
        "pointers" => pointers::run(),
        "server" => server::run(),
        "threadpool" => threadpool::run(),
        "json" => json::run(),
        _ => {
            println!("Unrecognized module: {}", args[1]);
            std::process::exit(-1);
        }
    }
}

fn main() {
    println!("Hello, world!");
    process_cmdline();
}
