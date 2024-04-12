# https://leetcode.com/problems/trapping-rain-water/


def trap(height: list[int]) -> int:
    lp = 0
    rp = len(height) - 1
    total = 0
    left_max = height[lp]
    right_max = height[rp]

    while lp < rp:
        if height[lp] < left_max:
            total += min(left_max, right_max) - height[lp]
        else:
            left_max = height[lp]

        if height[rp] < right_max:
            total += min(left_max, right_max) - height[rp]
        else:
            right_max = height[rp]

        if height[lp] <= height[rp]:
            lp += 1
        else:
            rp -= 1

    return total


def expect(height, ans):
    print(f"expect {height} == {ans}")
    v = trap(height)
    print(f"got {height} == {v}")
    assert v == ans


expect([4, 2, 0, 3, 2, 5], 9)
expect([0, 1, 0, 2, 1, 0, 1, 3, 2, 1, 2, 1], 6)
