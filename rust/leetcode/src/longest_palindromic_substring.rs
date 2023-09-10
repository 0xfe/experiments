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
pub fn longest_palindrome_slow(s: String) -> String {
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

// O(n^2) solution
pub fn longest_palindrome(s: String) -> String {
    let mut longest = (1, &s[0..1]);

    for i in 0..s.len() - 1 {
        let mut p1 = i;
        let mut p2 = i;

        let mut same = true;

        while p1 > 0 || p2 < s.len() - 1 {
            if p1 > 0 && p2 < s.len() - 1 {
                p1 -= 1;
                p2 += 1;

                if s.as_bytes()[p1] as char == s.as_bytes()[p2] as char {
                    if s.as_bytes()[p1] != s.as_bytes()[p1 + 1] {
                        same = false;
                    }
                    if (p2 - p1 + 1) > longest.0 {
                        longest = (p2 - p1 + 1, &s[p1..=p2]);
                    }
                    continue;
                }

                p1 += 1;
                p2 -= 1;
            }

            if p1 > 0 {
                p1 -= 1;

                if s.as_bytes()[p1] as char == s.as_bytes()[p2] as char && same {
                    if s.as_bytes()[p1] != s.as_bytes()[p1 + 1] {
                        same = false;
                        continue;
                    }

                    if (p2 - p1 + 1) > longest.0 {
                        longest = (p2 - p1 + 1, &s[p1..=p2]);
                    }
                    continue;
                }
                p1 += 1;
            }

            if p2 < s.len() - 1 {
                p2 += 1;
                if s.as_bytes()[p1] as char == s.as_bytes()[p2] as char && same {
                    if s.as_bytes()[p2] != s.as_bytes()[p2 - 1] {
                        same = false;
                        continue;
                    }

                    if (p2 - p1 + 1) > longest.0 {
                        longest = (p2 - p1 + 1, &s[p1..=p2]);
                    }
                    continue;
                }
                // not needed: p2 -= 1;
            }

            break;
        }
    }

    longest.1.into()
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
        assert_eq!(longest_palindrome("abb".to_string()), "bb".to_string());
        assert_eq!(
            longest_palindrome("tattarrattat".to_string()),
            "tattarrattat".to_string()
        );
        assert_eq!(
            longest_palindrome("aaaabaaa".to_string()),
            "aaabaaa".to_string()
        );
        assert_eq!(
            longest_palindrome("xaabacxcabaaxcabaax".to_string()),
            "xaabacxcabaax".to_string()
        );
        assert_eq!(longest_palindrome("abcba".to_string()), "abcba".to_string());
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
        assert_eq!(
            longest_palindrome("aacabdkacaa".to_string()),
            "aca".to_string()
        );
    }
}
