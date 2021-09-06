pub fn run() {
    let numbers: [i32;5] = [1, 2, 3, 4, 5];

    let mut n = 0;
    while n < numbers.len() {
        println!("Number {}", numbers[n]);
        n += 1;
    }

    println!("Numbers {:?}", numbers);

    let mut vnumbers : Vec<i32> = vec![1, 3, 5, 7, 9];
    vnumbers.push(11);
    println!("Vector of numbers: {:?}", vnumbers);

    vnumbers.pop();
    println!("Vector of numbers: {:?}", vnumbers);

    for x in vnumbers.iter() {
        println!("Got: {}", x)
    }

    for x in vnumbers.iter_mut() {
        *x *= 2;
    }
    println!("Post mutation: {:?}", vnumbers);

    println!("Starting loop loop... ");
    let mut count = 0;
    loop {
        count += 1;
        println!("{}", count);
        if count == 5 {
            break;
        }
    }

    for x in 0..5 {
        println!("for {}", x);
    }
}