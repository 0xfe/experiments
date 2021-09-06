pub fn run() {
    let mut s = String::from("heyoooo ");

    s.push_str("foo!");

    println!("{} {}", s, s.len());

    if s.is_empty() {
        println!("empty!")
    } else {
        println!("not empty!")
    }

    // Immutable primitive string type
    let imm_s : &str = "boo";
    println!("Immutable: {}", imm_s);
}
