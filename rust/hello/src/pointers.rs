use std::ops::Deref;

struct MyBox<T>(T);

impl<T> MyBox<T> {
    fn new(t: T) -> MyBox<T> {
        return MyBox(t);
    }
}

impl<T> Deref for MyBox<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        return &self.0;
    }
}

pub fn run() {
    let i = MyBox::new(25);

    println!("i = {}", *i);
}
