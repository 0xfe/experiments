#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ListNode {
    pub val: i32,
    pub next: Option<Box<ListNode>>,
}

impl ListNode {
    #[inline]
    fn new(val: i32) -> Self {
        ListNode { next: None, val }
    }
}

pub fn new_list(nums: Vec<i32>) -> Option<Box<ListNode>> {
    let mut list = None;

    for num in nums.into_iter().rev() {
        let mut node = ListNode::new(num);
        node.next = list;
        list = Some(Box::new(node));
    }

    list
}

pub fn from_int(num: i32) -> Option<Box<ListNode>> {
    let mut num = num;
    let mut list = vec![];

    while num > 0 {
        let digit = num % 10;
        num /= 10;

        list.push(digit);
    }

    new_list(list)
}

pub fn to_int(list: Option<Box<ListNode>>) -> i32 {
    let mut list = list;
    let mut num = 0;
    let mut i = 0;

    while let Some(node) = list {
        num += node.val * 10_i32.pow(i);
        i += 1;
        list = node.next;
    }

    num
}

pub fn add_two_numbers(
    l1: Option<Box<ListNode>>,
    l2: Option<Box<ListNode>>,
) -> Option<Box<ListNode>> {
    let mut l1 = l1.unwrap();
    let mut l2 = l2.unwrap();

    let mut sum = (l1.val + l2.val) % 10;
    let mut carry = (l1.val + l2.val) / 10;

    let mut solution = Box::new(ListNode::new(sum));
    let mut s = &mut solution;

    while l1.next.is_some() || l2.next.is_some() {
        l1 = l1.next.unwrap_or(Box::new(ListNode::new(0)));
        l2 = l2.next.unwrap_or(Box::new(ListNode::new(0)));

        sum = (l1.val + l2.val + carry) % 10;
        carry = (l1.val + l2.val + carry) / 10;

        s.next = Some(Box::new(ListNode::new(sum)));
        s = s.next.as_mut().unwrap();
    }

    if carry > 0 {
        s.next = Some(Box::new(ListNode::new(carry)));
    }

    Some(solution)
}

pub fn add_two_numbers2(
    l1: Option<Box<ListNode>>,
    l2: Option<Box<ListNode>>,
) -> Option<Box<ListNode>> {
    let num1 = to_int(l1);
    let num2 = to_int(l2);

    from_int(num1 + num2)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let l1 = from_int(342);
        let l2 = from_int(465);

        let solution = add_two_numbers(l1, l2);
        assert_eq!(to_int(solution), 807);

        let solution = add_two_numbers(from_int(1234), from_int(5678));
        assert_eq!(to_int(solution), 6912);

        let solution = add_two_numbers(from_int(1234), from_int(567));
        assert_eq!(to_int(solution), 1801);

        let solution = add_two_numbers(from_int(9999999), from_int(9999));
        assert_eq!(to_int(solution), 10009998);
        let solution = add_two_numbers2(from_int(9999999), from_int(9999));
        assert_eq!(to_int(solution), 10009998);
    }
}
