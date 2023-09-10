use std::collections::HashMap;

// https://leetcode.com/problems/longest-substring-without-repeating-characters/description/

pub fn length_of_longest_substring(s: String) -> i32 {
    let mut longest = 0;
    let mut current = 0;

    let mut set = HashMap::new();

    for (i, c) in s.chars().enumerate() {
        if let Some(v) = set.get(&c) {
            current = i - v;
            let t = *v;
            set.retain(|_, val| *val >= t);
            set.insert(c, i);
        } else {
            set.insert(c, i);
            current += 1;
        }

        if current > longest {
            longest = current;
        }
    }

    longest as i32
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(length_of_longest_substring("abcabcbb".to_string()), 3);
        assert_eq!(length_of_longest_substring("bbbbb".to_string()), 1);
        assert_eq!(length_of_longest_substring("pwwkew".to_string()), 3);
        assert_eq!(length_of_longest_substring("".to_string()), 0);
        assert_eq!(length_of_longest_substring(" ".to_string()), 1);
        assert_eq!(length_of_longest_substring("au".to_string()), 2);
        assert_eq!(length_of_longest_substring("dvdf".to_string()), 3);
        assert_eq!(length_of_longest_substring("abba".to_string()), 2);
        assert_eq!(length_of_longest_substring("tmmzuxt".to_string()), 5);
    }
}
