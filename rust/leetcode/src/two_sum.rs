// https://leetcode.com/problems/two-sum/description

use std::collections::HashMap;

// Premature optimization, slow and high memory
pub fn two_sum(nums: Vec<i32>, target: i32) -> Vec<i32> {
    let mut nums: Vec<(usize, i32)> = nums.into_iter().enumerate().collect();

    nums.sort_by(|a, b| a.1.cmp(&b.1));

    for (i, num) in nums.iter().enumerate() {
        for num2 in nums.iter().skip(i + 1) {
            // println!("{} {} {} {}", i, j, num, num2);
            if num.1 + num2.1 == target {
                return vec![num.0 as i32, num2.0 as i32];
            }

            if num.1 + num2.1 > target {
                break;
            }
        }
    }

    vec![0, 0]
}

// Lowest memory solution, slow
pub fn two_sum2(nums: Vec<i32>, target: i32) -> Vec<i32> {
    for i in 0..nums.len() {
        for j in i + 1..nums.len() {
            if nums[i] + nums[j] == target {
                return vec![i as i32, j as i32];
            }
        }
    }

    panic!()
}

// Fastest solution, higher memory
pub fn two_sum3(nums: Vec<i32>, target: i32) -> Vec<i32> {
    let mut map = HashMap::with_capacity(nums.len());

    for (i, num) in nums.iter().enumerate() {
        let complement = target - num;

        if let Some(&j) = map.get(&complement) {
            return vec![i as i32, j as i32];
        }

        map.insert(num, i);
    }

    panic!()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let a = vec![1, 2, 3];
        assert_eq!(two_sum(a.clone(), 3), vec![0, 1]);
        assert_eq!(two_sum2(a, 3), vec![0, 1]);

        let a = vec![1, 5, 12, 43, 3, 2, 22, 3433, 28, 21, 11, 9, 8, 7, 44];
        assert_eq!(two_sum(a.clone(), 24), vec![5, 6]);
        assert_eq!(two_sum2(a.clone(), 24), vec![4, 9]);

        // two_sum3 uses hashmaps, so the order of the vector is not guaranteed
        assert!(two_sum3(a.clone(), 24).contains(&5));
        assert!(two_sum3(a, 24).contains(&6));
    }
}
