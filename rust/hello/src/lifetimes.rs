fn gt<'a>(a: &'a str, b: &'a str) -> &'a str {
    if a > b {
        a
    } else {
        b
    }
}

pub fn run() {
    println!("gt {}", gt("foo", "bar"))
}