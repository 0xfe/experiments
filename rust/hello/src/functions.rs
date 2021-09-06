fn greet(text: &str, name: &str) {
    println!("{}, {}!", text, name);
}

fn add(a: i32, b: i32) -> i32 {
    a + b
}

pub fn run() {
    println!("TEST: functions.rs");
    greet("Hello", "Mo");

    println!("Sum: {}", add(4,5));

    let adder = |a: i32, b: i32| a + b;
    println!("Adder: {}", adder(4,5));
}