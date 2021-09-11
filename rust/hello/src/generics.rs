// You can use "where" to remove the generic type clutter form the
// function signature
fn add<T>(a: T, b: T) -> T
where T: std::ops::Add<Output=T> + std::fmt::Debug
{
    println!("{:?} + {:?}", a, b);
    return a + b
}

pub fn run() {
    add(4, 5);
}