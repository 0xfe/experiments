# https://leetcode.com/problems/merge-k-sorted-lists/

from heapq import heappop, heappush, heapify, heappushpop


# From leetcode
class ListNode:
    def __init__(self, val=0, next=None):
        self.val = val
        self.next = next

    def __repr__(self):
        me = self
        vals = []
        while me is not None:
            vals.append(me.val)
            me = me.next

        return repr(vals)


# Paste below


def __lt__(self, other):
    return self.val < other.val


ListNode.__lt__ = __lt__


def mergeKLists(lists: list[ListNode]) -> ListNode:
    lists = list(filter(lambda v: v is not None, lists))

    if len(lists) == 0:
        return None

    if len(lists) == 1:
        return lists[0]

    heapify(lists)
    top = heappop(lists)
    cur = top

    while len(lists) > 0:
        next_head = lists[0]
        while cur.next is not None and cur.next.val < next_head.val:
            cur = cur.next

        if cur.next is not None:
            cur.next = heappushpop(lists, cur.next)
        else:
            cur.next = heappop(lists)

        cur = cur.next

    return top


def to_list_node(nums: list) -> ListNode:
    if len(nums) == 0:
        return None

    head = ListNode(nums[0])
    cur = head
    for num in nums[1:]:
        cur.next = ListNode(num)
        cur = cur.next

    return head


def to_list(head: ListNode) -> list:
    if head is None:
        return []

    nums = []
    cur = head

    while cur is not None:
        nums.append(cur.val)
        cur = cur.next

    return nums


def to_list_of_listnodes(num_lists: list[list]):
    lists = []
    for nums in num_lists:
        lists.append(to_list_node(nums))

    return lists


def test_merge_eq(lists: list[list], expected) -> list:
    print("\nmergeKLists", lists)
    ans = to_list(mergeKLists(to_list_of_listnodes(lists)))
    print("  ", ans)
    return ans == expected
    return ans


assert test_merge_eq([[1, 4, 5], [1, 3, 4], [2, 6]], [1, 1, 2, 3, 4, 4, 5, 6])
assert test_merge_eq([], [])
assert test_merge_eq([[]], [])
assert test_merge_eq([[], []], [])
