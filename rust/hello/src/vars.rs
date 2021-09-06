pub fn run() {
    let mut blah = "foo";
    println!("{}", blah);
    blah = "bar";
    println!("{}", blah);

    // Constants should have explicit types
    const ID: i32 = 001;
    println!("{}", ID);

    let (foo, bar) = ("hi", "bye");
    println!("{} and {}", foo, bar);
}
