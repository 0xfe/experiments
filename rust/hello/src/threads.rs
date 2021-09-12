use std::{thread, time};

pub fn run() {
    let tid = thread::spawn(move || {
        for x in 0..10 {
            println!("{}", x);
            thread::sleep(time::Duration::from_millis(10));
        }
    });

    tid.join().unwrap();
}