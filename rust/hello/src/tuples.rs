pub fn run() {
    let tup: (&str, &str, i32) = ("mo", "by", 42);

    println!("{} {} {}", tup.0, tup.1, tup.2);
}