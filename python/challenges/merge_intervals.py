# https://leetcode.com/problems/merge-intervals/


def merge(intervals: list[list[int]]) -> list[list[int]]:
    if len(intervals) < 2:
        return intervals

    answer = []
    intervals.sort(key=lambda v: v[0])

    low, high = intervals[0]
    for r in intervals[1:]:
        if r[0] > high:
            answer.append([low, high])
            low, high = r[0], r[1]
        elif r[0] >= low and r[1] > high:
            high = r[1]

    answer.append([low, high])
    return answer


assert merge([[1, 3], [2, 6], [8, 10], [15, 18]]) == [[1, 6], [8, 10], [15, 18]]
assert merge([[2, 6], [1, 3], [1, 2], [8, 10], [15, 18]]) == [[1, 6], [8, 10], [15, 18]]
assert merge([[1, 3]]) == ([[1, 3]])
