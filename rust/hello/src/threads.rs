use std::sync::mpsc;
use std::sync::{Arc, Condvar, Mutex};
use std::{thread, time};

fn channels() {
    let (tx, rx) = mpsc::channel();

    let tid = thread::spawn(move || {
        for i in 0..10 {
            println!("Sending {}", i);
            tx.send(i).unwrap();
            thread::sleep(time::Duration::from_millis(10));
        }
    });

    thread::spawn(move || loop {
        match rx.recv() {
            Ok(i) => println!("Received {}", i),
            Err(e) => {
                // closed channel
                println!("Got expected error: {}", e);
                break;
            }
        }
    });

    tid.join().unwrap();
}

// Synchronized threads
fn sync_threads() {
    // Goal:
    // . Spawn two threads at the same time
    // . Second thread blocks until other thread is done
    // . Use mutex and condvar to synchronize the threads

    // This is the mutex and condvar wrapped in a tuple, and then in
    // an Atomic Refcounted Pointer (Arc)
    let pair = Arc::new((Mutex::new(false), Condvar::new()));
    let num_runs = 10;

    // This creates a new ref of the tuple (and increases the refcount). Since the
    // closure has "move" on it, the thread below takes ownership of the ref.
    let pair1 = Arc::clone(&pair);

    // Spawn a thread, and specify "move" on the closure, which causes
    // it to take ownership of any referred variables.
    let tid = thread::spawn(move || {
        for x in 0..num_runs {
            println!("Thread 1 {}", x);
            thread::sleep(time::Duration::from_millis(10));
        }

        // Deref the Arc pointer, then take a ref to the tuple
        let (lock, cv) = &*pair1;

        // lock() returns a MutexGuard, which is a scoped (RAII) mutex.
        let mut done = lock.lock().unwrap();

        // Deref because wrapped in MutexGuard
        *done = true;

        // Notify that we're done
        cv.notify_one();

        // lock released because MutexGuard is scoped.
    });

    let pair2 = Arc::clone(&pair);
    let tid2 = thread::spawn(move || {
        {
            // Deref the Arc pointer, then take a ref to the tuple
            let (lock, cv) = &*pair2;

            // lock() returns a MutexGuard, which is a scoped (RAII) mutex.
            let mut done = lock.lock().unwrap();

            // Deref, check if we got the signal
            while !*done {
                // This blocks and returns the MutexGuard if there's a signal
                done = cv.wait(done).unwrap();
            }

            // lock released at close of scope
        }

        for x in 0..num_runs {
            println!("Thread 2 {}", x);
            thread::sleep(time::Duration::from_millis(10));
        }
    });

    tid.join().unwrap();
    tid2.join().unwrap();
}

pub fn run() {
    channels();
    sync_threads();
}
