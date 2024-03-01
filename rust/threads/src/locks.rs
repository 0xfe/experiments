use std::{sync::Mutex, thread};

fn mutexes() {
    struct M {
        foo: String,
        bar: Mutex<i32>,
    }

    let m = M {
        foo: "hello".to_string(),
        bar: Mutex::new(0),
    };

    thread::scope(|s| {
        for _ in 0..10 {
            s.spawn(|| {
                let mut b = m.bar.lock().unwrap();
                *b += 1;
                m.foo = "world".to_string();
            });
        }
    });
}

pub fn run() {
    mutexes()
}
