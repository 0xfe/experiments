# https://leetcode.com/problems/find-median-from-data-stream/

from heapq import heappop, heappush, heappushpop

# Python heapq supports only min-heap. If you
# want a max-heap, then negate the elements.


class MedianFinder:
    def __init__(self):
        self.median = None
        self.count = 0
        self.left = []  # max heap, stores negative elements
        self.right = []  # min heap, stores positive elements
        pass

    def addNum(self, num: int) -> None:
        if self.count == 0:
            self.median = num
        else:
            if self.median is not None:
                if num > self.median:
                    heappush(self.left, -self.median)
                    heappush(self.right, num)
                else:
                    heappush(self.right, self.median)
                    heappush(self.left, -num)

                self.median = None
            else:
                left_tail = -self.left[0]
                right_head = self.right[0]
                median = (left_tail + right_head) / 2

                if num > median:
                    self.median = heappushpop(self.right, num)
                else:
                    self.median = -heappushpop(self.left, -num)

        # print("M", self.count, self.left, self.median, self.right)
        self.count += 1

    def findMedian(self) -> float:
        if self.median is not None:
            return self.median
        else:
            return (-self.left[0] + self.right[0]) / 2


# Your MedianFinder object will be instantiated and called as such:
# obj = MedianFinder()
# obj.addNum(num)
# param_2 = obj.findMedian()


def test(commands, values, expected):
    print(
        "\nNew Test:",
        "\n".join(map(lambda v: f"  {v[0]} {v[1]}", list(zip(commands, values)))),
    )
    finder = None
    result = []

    for c, v in zip(commands, values):
        match c:
            case "MedianFinder":
                finder = MedianFinder()
                result = [None]

            case "addNum":
                result.append(finder.addNum(v[0]))

            case "findMedian":
                result.append(finder.findMedian())

    print("  --> want:", expected)
    print("  --> got:", result)
    return result == expected


assert test(
    ["MedianFinder", "addNum", "addNum", "findMedian", "addNum", "findMedian"],
    [[], [1], [2], [], [3], []],
    [None, None, None, 1.50000, None, 2.00000],
)
