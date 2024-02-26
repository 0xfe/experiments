use std::thread;

pub fn run1() {
    let h1 = thread::spawn(f);
    let h2 = thread::spawn(f);

    h1.join().unwrap();
    h2.join().unwrap();
    println!("Hello, from main thread ID: {:?}", thread::current().id());
}

pub fn run2() {
    let nums = vec![1, 2, 3, 4, 5];

    thread::spawn(move || {
        for n in nums {
            println!("{:?} says: {}", thread::current().id(), n);
        }
    })
    .join()
    .unwrap();
}

pub fn run3() {
    // Scoped threads are a way to ensure that the spawned thread does not outlive the parent thread. Unilke
    // regular threads spawned with thread::spawn, which have static lifetimes, scoped threads have lifetimes
    // that are tied to the parent thread. This means that the parent thread will wait for all of its scoped
    // threads to finish before it exits. This is useful when you want to ensure that all threads are done
    // before the parent thread exits.

    let nums = vec![1, 2, 3, 4, 5];

    thread::scope(|s| {
        s.spawn(|| {
            for n in nums {
                println!("scoped {:?} says: {}", thread::current().id(), n);
            }
        });
    })
}

pub fn run4() {
    // Sharing values with threads
    static X: [i32; 3] = [1, 2, 3];

    let x = Box::leak(Box::new([1, 2, 3]));

    thread::spawn(|| dbg!(&X)).join().unwrap();
    thread::spawn(|| dbg!(X)).join().unwrap();
    thread::spawn(|| dbg!(x)).join().unwrap();
}

pub fn run5() {
    // Some refcounting
    use std::rc::Rc;

    let rc = Rc::new([1, 2, 3]);
    let rc2 = rc.clone();

    println!("rc: {:?} rc2: {:?}", rc.as_ptr(), rc2.as_ptr());

    use std::sync::Arc;
    let arc = Arc::new([1, 2, 3]);
    let arc2 = arc.clone();

    println!("arc: {:?} arc2: {:?}", arc.as_ptr(), arc2.as_ptr());

    thread::spawn({
        // this is not a closure (note: no ||), it's just a separate scope
        let arc = arc.clone();
        move || {
            // this is a closure
            println!("arc: {:?}", arc.as_ptr());
        }
    })
    .join()
    .unwrap();
}


// MyStruct must be copyable to be used with Cell
#[derive(Debug, Clone, Copy)]
struct MyStruct {
    a: i32,
    b: i32,
}

pub fn run6() {
    use std::cell::Cell;

    let c = Cell::new(MyStruct { a: 1, b: 2 });
    let v = c.get();
    println!("v: {:?} {:?}", v.a, v.b);
}

fn f() {
    let id = thread::current().id();
    println!("Hello, from thread ID: {:?}", id);
}
