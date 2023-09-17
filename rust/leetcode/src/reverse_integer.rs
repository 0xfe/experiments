// https://leetcode.com/problems/reverse-integer/

#[allow(clippy::result_unit_err)]
pub fn reverse2(x: i32) -> Result<i32, ()> {
    println!("\nx: {}", x);
    let mut result = 0i32;
    let mut started = None;

    let mut final_place = false;
    for i in (1..10).rev() {
        let place = 10i32.checked_pow(i).ok_or(())?;
        let digit = (x % place) / 10i32.pow(i - 1);

        if i == 9 && x / place != 0 {
            final_place = true;
        }

        if digit != 0 && started.is_none() {
            started = Some(i);
        }

        if let Some(started) = started {
            let multiple = digit
                .checked_mul(10i32.checked_pow(started - i).ok_or(())?)
                .ok_or(())?;

            result = result.checked_add(multiple).ok_or(())?;
        }
    }

    if final_place {
        result = result.checked_mul(10).ok_or(())?;
    }

    Ok(result)
}

pub fn reverse(x: i32) -> i32 {
    reverse2(x).unwrap_or(0)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_7() {
        assert_eq!(reverse(123), 321);
        assert_eq!(reverse(-123), -321);
        assert_eq!(reverse(120), 21);
        assert_eq!(reverse(102), 201);
        assert_eq!(reverse(-1002), -2001);
        assert_eq!(reverse(0), 0);
        assert_eq!(reverse(1_534_236_469), 0);
        assert_eq!(reverse(-1_534_236_469), 0);
        assert_eq!(reverse(-2147483412), -2143847412);
    }
}
