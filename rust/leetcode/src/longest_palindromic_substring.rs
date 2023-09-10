// https://leetcode.com/problems/longest-palindromic-substring/

#[inline]
pub fn is_palindrome(s: &str) -> bool {
    let max_len = s.len() / 2 + 1;
    for (i, c) in s.chars().enumerate() {
        if c != s.as_bytes()[s.len() - i - 1] as char {
            return false;
        }

        if i >= max_len {
            break;
        }
    }

    true
}

// Naive solution - O(n^3)
pub fn longest_palindrome(s: String) -> String {
    let start_len = s.len();

    if start_len == 0 {
        return "".to_string();
    }

    for len in (1..=start_len).rev() {
        for i in 0..=start_len - len {
            let sub = &s[i..i + len];
            if is_palindrome(sub) {
                return sub.to_string();
            }
        }
    }

    unreachable!()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn is_palindrome_test() {
        assert!(!is_palindrome("foobar"));
        assert!(is_palindrome("aba"));
        assert!(is_palindrome("abba"));
        assert!(is_palindrome("cabac"));
        assert!(is_palindrome("cabbac"));
        assert!(!is_palindrome("cabcac"));
    }

    #[test]
    fn longest_palindrome_test() {
        assert_eq!(longest_palindrome("babad".to_string()), "bab".to_string());
        assert_eq!(longest_palindrome("cbbd".to_string()), "bb".to_string());
        assert_eq!(longest_palindrome("a".to_string()), "a".to_string());
        assert_eq!(longest_palindrome("ac".to_string()), "a".to_string());
        assert_eq!(longest_palindrome("bb".to_string()), "bb".to_string());
        assert_eq!(longest_palindrome("ccc".to_string()), "ccc".to_string());
        assert_eq!(longest_palindrome("aaaa".to_string()), "aaaa".to_string());
        assert_eq!(longest_palindrome("aaaaa".to_string()), "aaaaa".to_string());
        assert_eq!(
            longest_palindrome("aaaaaa".to_string()),
            "aaaaaa".to_string()
        );
        assert_eq!(
            longest_palindrome("aaaaaaa".to_string()),
            "aaaaaaa".to_string()
        );
    }
}
