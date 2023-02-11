pub struct ThreadPool {
    size: usize,
}

impl ThreadPool {
    fn new(size: usize) -> ThreadPool {
        assert!(size > 0);
        ThreadPool { size }
    }

    fn size(&self) -> usize {
        return self.size;
    }
}

pub fn run() {
    println!("running threadpool");
    let tp = ThreadPool::new(10);
    println!("size: {}", tp.size());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_pool() {
        let tp = ThreadPool::new(5);
        assert_eq!(tp.size(), 5);
    }
}
