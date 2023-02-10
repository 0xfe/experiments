use std::fmt::Debug;
use std::ops::Deref;

#[derive(Debug)]
struct MyBox<T: Debug>(T);

impl<T: Debug> MyBox<T> {
    fn new(t: T) -> MyBox<T> {
        return MyBox(t);
    }
}

impl<T: Debug> Deref for MyBox<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        return &self.0;
    }
}

impl<T: Debug> Drop for MyBox<T> {
    fn drop(&mut self) {
        println!("goodbye {:?}!", *self)
    }
}

pub fn run() {
    let i = MyBox::new(25);
    println!("i = {}", *i);

    let j = MyBox::new(30);
    drop(j);
}
