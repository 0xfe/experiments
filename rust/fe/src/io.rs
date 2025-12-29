pub const STDIO_PATH: &str = "-";

pub fn is_stdio(path: &str) -> bool {
    path == STDIO_PATH
}

#[cfg(test)]
mod tests {
    use super::is_stdio;

    #[test]
    fn detects_stdio_path() {
        assert!(is_stdio("-"));
        assert!(!is_stdio("file.txt"));
    }
}
