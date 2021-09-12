pub fn run() {
    // Create some arrays and vectors and iterate using different kinds of loops.

    // Arrays are fixed-size. You can't add or remove elements.
    let numbers: [i32; 5] = [1, 2, 3, 4, 5];

    let mut n = 0;
    while n < numbers.len() {
        println!("Number {}", numbers[n]);
        n += 1;
    }

    println!("Numbers {:?}", numbers);

    // Vectors can be dynamically appended to, resized, etc.
    let mut vnumbers: Vec<i32> = vec![1, 3, 5, 7, 9];
    vnumbers.push(11);
    println!("Vector of numbers: {:?}", vnumbers);

    vnumbers.pop();
    println!("Vector of numbers: {:?}", vnumbers);

    // You can iterate with iter() and iter_mut()
    for x in vnumbers.iter() {
        println!("Got: {}", x)
    }

    for x in vnumbers.iter_mut() {
        *x *= 2;
    }
    println!("Post mutation: {:?}", vnumbers);

    // You can iterate with loop
    println!("Starting loop loop... ");
    let mut count = 0;
    loop {
        count += 1;
        println!("{}", count);
        if count == 5 {
            break;
        }
    }

    // You can terate with "for in"
    for x in 0..5 {
        println!("for {}", x);
    }
}
