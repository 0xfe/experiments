use async_std::task;
use futures::executor::block_on;
use std::time;

async fn part1() -> usize {
    for n in 1..10 {
        // Notice this is async_std::task::sleep and not thread::sleep. Also
        // notice the await. You need to use async-aware blocking methods wherever
        // possible.
        task::sleep(time::Duration::from_millis(100)).await;
        println!("a - {}", n);
    }

    1
}

async fn part2() -> usize {
    for n in 1..10 {
        task::sleep(time::Duration::from_millis(50)).await;
        println!("b - {}", n);
    }

    2
}

async fn go() {
    let (a, b) = futures::join!(part1(), part2());
    println!("a: {}, b: {}", a, b);
}

pub fn run() {
    block_on(go());
}
