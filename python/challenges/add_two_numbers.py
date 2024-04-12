# https://leetcode.com/problems/add-two-numbers/


class ListNode:
    @staticmethod
    def from_list(vals: list):
        ans = ListNode(vals[0])
        node = ans

        for val in vals[1:]:
            node.next = ListNode(val)
            node = node.next

        return ans

    def __init__(self, val, next=None):
        self.val = val
        self.next = next

    def to_list(self):
        ans = []
        node = self

        while node is not None:
            print(node.val, node.next)
            ans.append(node.val)
            node = node.next

        return ans


def addTwoNumbers(l1: ListNode, l2: ListNode) -> ListNode:
    final = ListNode(0)
    ans = final
    p1 = l1
    p2 = l2
    carry = 0

    while 1:
        val = (p1 and p1.val or 0) + (p2 and p2.val or 0) + carry
        ans.val = int(val % 10)
        carry = int(val / 10)
        p1 = p1 and p1.next
        p2 = p2 and p2.next

        if p1 is None and p2 is None and carry == 0:
            break

        ans.next = ListNode(0)
        ans = ans.next

    return final


l = ListNode.from_list
ans = addTwoNumbers(l([2, 4, 3]), l([5, 6, 4]))
print(ans, ans.to_list())


ans = addTwoNumbers(l([9, 9, 9, 9, 9, 9, 9]), l([9, 9, 9, 9]))
print(ans, ans.to_list())
