// https://leetcode.com/problems/median-of-two-sorted-arrays/description/

pub fn find_median_sorted_arrays(nums1: Vec<i32>, nums2: Vec<i32>) -> f64 {
    let mut i = 0;
    let mut j = 0;

    let len = nums1.len() + nums2.len();
    assert_ne!(len, 0);

    let is_even = len % 2 == 0;

    let mut target = if len > 1 { len / 2 } else { 0 };
    if is_even {
        target -= 1;
    }

    println!("\n{} {} {:?} {:?}", target, is_even, nums1, nums2);

    if nums1.is_empty() {
        if nums2.is_empty() {
            return 0.0;
        } else {
            return if is_even {
                (nums2[target] + nums2[target + 1]) as f64 / 2.0
            } else {
                nums2[target] as f64
            };
        }
    } else if nums2.is_empty() {
        return if is_even {
            (nums1[target] + nums1[target + 1]) as f64 / 2.0
        } else {
            nums1[target] as f64
        };
    }

    let mut a = 0;

    while i + j <= target {
        if i > nums1.len() - 1 {
            a = nums2[j];
            j += 1;
            continue;
        } else if j > nums2.len() - 1 {
            a = nums1[i];
            i += 1;
            continue;
        }

        if nums1[i] < nums2[j] {
            a = nums1[i];
            i += 1;
        } else {
            a = nums2[j];
            j += 1;
        }
    }

    let mut b;

    if i > nums1.len() - 1 {
        b = nums2[j];
    } else if j > nums2.len() - 1 {
        b = nums1[i];
    } else {
        if nums1[i] < nums2[j] {
            b = nums1[i];
        } else {
            b = nums2[j];
        }
    }

    println!("{} {} {} {}", i, j, a, b);

    if is_even {
        (a + b) as f64 / 2.0
    } else {
        a as f64
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(find_median_sorted_arrays(vec![1, 3], vec![2]), 2.0);
        assert_eq!(find_median_sorted_arrays(vec![1, 2], vec![3, 4]), 2.5);
        assert_eq!(find_median_sorted_arrays(vec![1], vec![]), 1.0);
        assert_eq!(find_median_sorted_arrays(vec![], vec![2]), 2.0);
        assert_eq!(find_median_sorted_arrays(vec![3], vec![-2, -1]), -1.0);
    }
}
