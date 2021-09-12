mod stack;

fn main() {
    println!("Hello, world!");

    let mut s = stack::MoStack::new();
    s.push(String::from("boo"));
}
