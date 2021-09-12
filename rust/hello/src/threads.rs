use std::{thread, time};
use std::sync::{Arc, Mutex, Condvar};

pub fn run() {
    // Goal:
    // . Spawn two threads at the same time
    // . Second thread blocks until other thread is done
    // . Use mutex and condvar to synchronize the threads

    // This is the mutex and condvar wrapped in a tuple, and then in
    // an Atomic Refcounted Pointer (Arc)
    let pair = Arc::new((Mutex::new(false), Condvar::new()));
    let num_runs = 10;

    // This creates a new ref of the tuple. Since the closure has "move" on
    // it, it takes ownership of the ref.
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

        // Get the MutexGuard for the lock
        let mut done = lock.lock().unwrap();

        // Deref because wrapped in MutexGuard
        *done = true;

        // Notify that we're done
        cv.notify_one();
    });

    let pair2 = Arc::clone(&pair);
    let tid2 = thread::spawn(move || {
        // Deref the Arc pointer, then take a ref to the tuple
        let (lock, cv) = &*pair2;

        // Get the MutexGuard for the lock
        let mut done = lock.lock().unwrap();

        // Deref, check if we got the signal
        while !*done {
            // This blocks and returns the MutexGuard if there's a signal
            done = cv.wait(done).unwrap();
        }

        for x in 0..num_runs {
            println!("Thread 2 {}", x);
            thread::sleep(time::Duration::from_millis(10));
        }
    });

    tid.join().unwrap();
    tid2.join().unwrap();
}