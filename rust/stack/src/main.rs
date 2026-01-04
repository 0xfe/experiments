mod stack;

fn main() {
    println!("Hello, world!");

    let mut s = stack::MoStack::new();
    s.push(String::from("boo"));
    s.push(String::from("foo"));

    println!("pop {}", s.pop().unwrap());
    println!("pop {}", s.pop().unwrap());
    println!("pop {}", s.pop().unwrap_or_else(|| String::from("empty")));

    println!("second stack");

    let first_item: i32 = 10;
    let mut items = [first_item; 10];
    let mut s2 = stack::MeStack::new(&mut items);
    s2.push(1);
    s2.push(2);
    println!("pop {}", s2.pop().unwrap());
    println!("pop {}", s2.pop().unwrap());
    println!("pop {}", s2.pop().unwrap_or(0));
}
