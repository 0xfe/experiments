use std::{cell::UnsafeCell, sync::atomic::AtomicUsize, thread, time::Duration};

pub fn unsafe_cell() {
    // Immutable
    let a = UnsafeCell::new([1, 2, 3]);

    let ptr = a.get();

    // Get and mutate it raw -- replace the whole thing
    unsafe {
        *ptr = [3, 4, 5];
    }

    println!("ptr: {:?} = {:?}", ptr, unsafe { *ptr });

    // This won't work because a is not mutable
    // let r = a.get_mut();

    let mut a = UnsafeCell::new([2, 1, 3]);
    let r = a.get_mut();

    (*r).sort();

    println!("r: {:?}", r);

    // With vec
    let mut a = UnsafeCell::new(vec![41, 200, 113]);
    let r = a.get_mut();

    r.push(50);
    r.sort();
    println!("r: {:?}", r);
}

pub fn atomic1() {
    println!("AtomicUsize");
    static COUNTER: AtomicUsize = AtomicUsize::new(0);

    // Every atomic operation takes an argument of type std::sync::atomic::Ordering,
    // which determines what guarantees we get about the relative ordering of operations.
    // The simplest variant with the fewest guarantees is Relaxed. Relaxed still
    // guarantees consistency on a single atomic variable, but does not promise anything
    // about the relative order of operations between different variables.

    thread::spawn(|| loop {
        let v = COUNTER.load(std::sync::atomic::Ordering::Relaxed);
        println!("thread 1: {}", v);
        if v >= 9 {
            break;
        }

        thread::sleep(Duration::from_millis(100));
    });

    for i in 0..10 {
        COUNTER.store(i, std::sync::atomic::Ordering::Relaxed);
        thread::sleep(Duration::from_millis(100));
    }
}

pub fn run() {
    unsafe_cell();
    atomic1();
}
