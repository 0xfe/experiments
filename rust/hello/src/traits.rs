
#[derive(Debug)]
struct Color {
    r: u8,
    g: u8,
    b: u8
}

// Method syntax
impl Color {
    pub fn new() -> Self {
        Self {r: 0, g:0, b:0}
    }

    fn rgb(&self) -> String {
        format!("{} {} {}", self.r, self.g, self.b)
    }

    fn set_r(&mut self, r: u8) -> &mut Self {
        self.r = r;
        self
    }

    fn calculate(&self) -> Result<u8, &str> {
        if self.r > 5 {
            Ok(self.r)
        } else {
            Err("too small")
        }
    }
}

trait Equals {
    fn equal(&self, t: &Self) -> bool;
}

impl Equals for Color {
    fn equal(&self, t: &Self) -> bool {
        self.r == t.r && self.g == t.g && self.b == t.b
    }
}

// Drop is an RAII trait. Run code when Color goes out
// of scope.
impl Drop for Color {
    fn drop(&mut self) {
        println!("Color drop!");
    }
}

pub fn run() {
    println!("{}", Color {r: 80, g: 80, b: 80}.rgb());

    let mut foo = Color::new();
    println!("{}", foo.set_r(200).rgb());

    match foo.calculate() {
        Ok(v) => println!("Ok {}", v),
        Err(e) => println!("Err {}", e),
    }

    // use if let which is simpler
    if let Ok(v) = foo.calculate() {
        println!("if let Ok: {}", v)
    }

    // use unwrap() to return value or panic if Err
    println!("v = {}", foo.calculate().unwrap());

    println!("same {}", foo.equal(&Color::new()));
    println!("same {}", Color::new().equal(&Color::new()));
    println!("derive Debug {:?}", foo);
}