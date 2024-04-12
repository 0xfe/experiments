# https://leetcode.com/problems/best-time-to-buy-and-sell-stock/description/


"""
Fastest algo
"""


def max_profit1(prices: list[int]) -> int:
    print("max_profit", prices)
    if len(prices) == 0:
        raise ValueError("empty list")

    min_price = 10000
    max_profit = 0

    for p in prices:
        min_price = min(min_price, p)
        max_profit = max(p - min_price, max_profit)

    return max_profit


"""
This is not really that fast
"""


def max_profit2(prices: list[int]) -> int:
    import itertools

    if len(prices) == 0:
        raise ValueError("empty list")

    def f(total, elem):
        minv = min(total[0], elem)
        return (minv, max(elem - minv, total[1]))

    *_, last = itertools.accumulate(prices, f, initial=(100000, 0))
    return last[1]


assert max_profit1([7, 1, 5, 3, 6, 4]) == 5
assert max_profit2([7, 1, 5, 3, 6, 4]) == 5
