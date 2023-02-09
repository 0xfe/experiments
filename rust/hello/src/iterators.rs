use std::env;

fn iterate() -> Vec<i32> {
    let v = vec![5, 4, 2, 1];

    for i in &v {
        println!("v {i}");
    }

    let mut total: i32 = v.iter().sum();
    println!("sum: {total}");

    // map() creates a new iterator, and because iterators are lazily evaluated,
    // you need to call collect() on it to iterate through and create a new vector.
    let n = 3;
    let v2: Vec<_> = v.iter().map(|x| x * n).collect();

    total = v2.iter().sum();
    println!("v2 sum: {total}");

    let v3: Vec<_> = v.into_iter().filter(|x| *x > 3).collect();
    total = v3.iter().sum();
    println!("v3 {:?} sum: {total}", v3);
    v3
}

fn show_values(iter: impl Iterator<Item = String>) {
    let values: Vec<_> = iter.collect();
    println!("{}", values.join(", "));
}

fn arguments() {
    show_values(env::args())
}

pub fn run() {
    iterate();
    arguments();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn iterate_test() {
        let v3 = iterate();
        assert_eq!(v3, vec![5, 4])
    }
}
