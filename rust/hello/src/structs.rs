struct Color {
    r: u8,
    g: u8,
    b: u8
}

impl Color {
    fn new(r: u8, g: u8, b: u8) -> Color {
        Color{r: r, g: g, b: b}
    }

    fn css(&self) -> String {
        format!("#{:02x}{:02x}{:02x}", self.r, self.g, self.b)
    }

    fn set_r(&mut self, v: u8) {
        self.r = v;
    }
}

pub fn run() {
    let c = Color{r: 5, g: 6, b: 7};
    println!("{} {} {}", c.r, c.g, c.b);

    // Tuple Struct
    struct OtherColor(u8, u8, u8);
    let uc = OtherColor(4, 5, 6);
    println!("{} {} {}", uc.0, uc.1, uc.1);

    let mut mycolor = Color::new(25, 68, 230);
    println!("{}", mycolor.css());

    mycolor.set_r(200);
    println!("{}", mycolor.css());
}