mod stack;

fn main() {
    println!("Hello, world!");

    let mut s = stack::MoStack::new();
    s.push(String::from("boo"));
    s.push(String::from("foo"));

    println!("pop {}", s.pop().unwrap());
    println!("pop {}", s.pop().unwrap());
    println!("pop {}", s.pop().unwrap());
}
