use std::cell::UnsafeCell;

pub fn run() {
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
