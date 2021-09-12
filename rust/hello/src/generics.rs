// You can use "where" to remove the generic type clutter form the
// function signature
fn add<T>(a: T, b: T) -> T
where T: std::ops::Add<Output=T> + std::fmt::Debug
{
    // The debug trait lets you do this
    println!("{:?} + {:?}", a, b);
    // The Add trait lets you do this
    return a + b
}

pub fn run() {
    add(4, 5);
}