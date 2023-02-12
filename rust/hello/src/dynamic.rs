#[derive(Debug)]
struct Point(i32, i32);

#[derive(Debug)]
struct Rect {
    tl: Point,
    br: Point,
}

#[derive(Debug)]
struct Circle {
    center: Point,
    radius: u32,
}

pub trait Draw {
    fn draw(&self);
}

impl Draw for Rect {
    fn draw(&self) {
        println!("Drawing rectangle: {:?}", self);
        println!("br: {:?} tl: {:?}", self.br, self.tl);
    }
}

impl Draw for Circle {
    fn draw(&self) {
        println!("Drawing circle: {:?}", self);
        println!("center: {:?} radius: {:?}", self.center, self.radius);
    }
}

pub fn draw_all(shapes: &Vec<Box<dyn Draw>>) {
    for shape in shapes {
        shape.draw();
    }
}

pub fn run() {
    let circle = Box::new(Circle {
        center: Point(100, 100),
        radius: 10,
    });

    let rect = Box::new(Rect {
        tl: Point(0, 0),
        br: Point(100, 100),
    });

    let shapes: Vec<Box<dyn Draw>> = vec![circle, rect];

    draw_all(&shapes);
}
