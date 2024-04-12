# https://leetcode.com/problems/maximum-subarray/

# [-2,1,-3,4,-1,2,1,-5,4]
#
# 0, 1, -1
# 0, 2, -4
# 1, 2, -2
# 1, 3, 2
# 1, 4, 1
# 2, 4, 0
# 3, 4, 3
# 3, 5, 5
# 3, 6, 6
# 3, 7, 1
# 4, 7, 5


def maxSubArray(nums) -> int:
    if len(nums) == 1:
        return nums[0]

    max_so_far = 0
    ans = -100000

    for n in nums:
        max_so_far += n

        if n > max_so_far:
            max_so_far = n

        if max_so_far > ans:
            ans = max_so_far

    print("ans", ans)
    return ans


def test_maxSubArray():
    assert maxSubArray([-2, 1, -3, 4, -1, 2, 1, -5, 4]) == 6
    assert maxSubArray([1]) == 1
    assert maxSubArray([5, 4, -1, 7, 8]) == 23
    assert maxSubArray([5, 4, -1, 7, -8]) == 15


test_maxSubArray()
