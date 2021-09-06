pub fn run() {
    let mut s = String::from("heyoooo ");

    s.push_str("foo!");

    println!("{} {}", s, s.len());

    if s.is_empty() {
        println!("empty!")
    } else {
        println!("not empty!")
    }
}
