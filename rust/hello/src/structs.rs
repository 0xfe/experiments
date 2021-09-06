struct Color {
    r: u8,
    g: u8,
    b: u8
}

pub fn run() {
    let c = Color{r: 5, g: 6, b: 7};
    println!("{} {} {}", c.r, c.g, c.b);

    // Tuple Struct
    struct OtherColor(u8, u8, u8);
    let uc = OtherColor(4, 5, 6);
    println!("{} {} {}", uc.0, uc.1, uc.1);
}