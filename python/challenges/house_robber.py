# https://leetcode.com/problems/house-robber/


def rob(nums: list[int]) -> int:
    highest = 0
    for i, v in enumerate(nums):
        if i in (0, 1):
            pass
        elif i == 2:
            nums[i] = v + nums[i - 2]
        else:
            nums[i] = v + max(nums[i - 2], nums[i - 3])

        highest = max(highest, nums[i])

    return highest


assert rob([1, 2, 3, 1]) == 4
assert rob([2, 7, 9, 3, 1]) == 12
