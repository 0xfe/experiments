fn gt<'a>(a: &'a str, b: &'a str) -> &'a str {
    if a > b {
        a
    } else {
        b
    }
}

pub fn run() {
    // Stack allocated int
    let a = 5;

    // Copy
    let b = a;

    // This is fine
    println!("{} {}", a, b);

    // Heap allocated int
    let a = Box::new(5);

    // This changes ownership of the int from a to b
    let b = a;

    // This won't complile because a is invalid.
    // println!("{} {}", a, b);

    // You can clone it instead. (Note b is still the owner, so clone from b)
    let c = b.clone();

    // This is fine
    println!("{} {}", b, c);

    println!("gt {}", gt("foo", "bar"))

    // b and c are destroyed when they go out of scope
}