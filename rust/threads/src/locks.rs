use std::{
    collections::VecDeque,
    sync::{Arc, Mutex},
    thread,
    time::Duration,
};

use rand::Rng;

fn mutexes() {
    struct M {
        bar: Mutex<i32>,
    }

    let m = M { bar: Mutex::new(0) };

    thread::scope(|s| {
        for _ in 0..10 {
            s.spawn(|| {
                let random_millis = rand::thread_rng().gen_range(0..100);
                thread::sleep(Duration::from_millis(random_millis));
                let mut b = m.bar.lock().unwrap();

                // Increment the value (so output is always 1, 2, 3, ...))
                *b += 1;
                println!("{}", *b);
            });
        }
    });
}

fn mutexes2() {
    struct M {
        bar: Arc<Mutex<i32>>,
    }

    let m = M {
        bar: Arc::new(Mutex::new(0)),
    };

    let mut handles = vec![];

    for i in 0..10 {
        let bar = Arc::clone(&m.bar);

        let handle = thread::spawn(move || {
            let random_millis = rand::thread_rng().gen_range(0..100);

            // sleep for a random amount of time
            thread::sleep(Duration::from_millis(random_millis));

            let mut b = bar.lock().unwrap();
            // Assign i to b, so output will vary based on the order of execution
            *b = i;
            println!("{}", *b);
        });

        handles.push(handle);
    }

    handles.into_iter().for_each(|h| h.join().unwrap());
}

fn park() {
    println!("Parking a thread");

    // Share this queue between the producer and consumer, using thread::park() to
    // block the consumer when the queue is empty. This is a simple way to implement
    // a producer-consumer pattern, but breaks down when there are multiple consumers: if
    // we had multiple consumer threads taking items from the same queue, the producer
    // thread would have no way of knowing which of the consumers is actually
    // waiting and should be woken up.
    let q = Mutex::new(VecDeque::new());

    thread::scope(|s| {
        // Consumer
        let t = s.spawn(|| loop {
            let item = q.lock().unwrap().pop_front();
            if let Some(v) = item {
                println!("Consumed {}", v);
                if v == 9 {
                    break;
                }
            } else {
                // Park only if the queue is empty, not on every iteration (otherwise it
                // may block the thread indefinitely waiting for an item to be pushed)
                thread::park();
            }
        });

        // Producer
        for i in 0..10 {
            q.lock().unwrap().push_back(i);
            t.thread().unpark();
            thread::sleep(Duration::from_millis(50));
        }
    });
}

fn cond_var() {
    println!("Condition variables");

    let q = Mutex::new(VecDeque::new());
    let cvar = std::sync::Condvar::new();

    thread::scope(|s| {
        // Consumer
        s.spawn(|| {
            loop {
                let mut q = q.lock().unwrap();
                let item = q.pop_front();
                if let Some(v) = item {
                    println!("Consumed {}", v);
                    if v == 9 {
                        break;
                    }
                } else {
                    // Wait for the producer to signal that there's an item in the queue
                    q = cvar.wait(q).unwrap();
                }
            }
        });

        for i in 0..10 {
            q.lock().unwrap().push_back(i);
            cvar.notify_one();
            thread::sleep(Duration::from_millis(50));
        }
    });
}

pub fn run() {
    mutexes();
    mutexes2();
    park();
    cond_var();
}
