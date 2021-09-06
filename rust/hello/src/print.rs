pub fn run() {
    let a = format!("blah");
    println!("{}", a);

    let pi = 3.14159;
    println!("{:1.3}", pi);

    #[derive(Debug)]
    struct MyType(i32);

    // Use debug print {:?}
    let b = MyType(4);
    println!("{:?}", b);

    println!("{} is from {}", "Mo", "Canada");
    println!("{0:b} {0:x}", 16233);
}
