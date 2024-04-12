# Working through the problem of finding all subsets of [1, 2, 3, 4, 5] in a recursive manner.

# ---- Base:
# []

# ---- Add 1:
# 1

# ---- Add 2:
# 2
# 1, 2

# ---- Add 3
# 1, 3
# 2, 3
# 1, 2, 3

# ---- Add 4
# 1, 4
# 2, 4
# 1, 2, 4

# 1, 3, 4
# 2, 3, 4
# 1, 2, 3, 4

# ---- Add 5
# 1, 5
# 2, 5
# 1, 2, 5

# 1, 3, 5
# 2, 3, 5
# 1, 2, 3, 5

# 1, 4, 5
# 2, 4, 5
# 1, 2, 4, 5

# 1, 3, 4, 5
# 2, 3, 4, 5
# 1, 2, 3, 4, 5

# Every line of numbers above is a unique subset of [1, 2, 3, 4, 5].
#
# [[], [1], [2], [1, 2], [3], [1, 3], [2, 3], [1, 2, 3], [4], [1, 4], [2, 4], [1, 2, 4], [3, 4], [1, 3, 4], [2, 3, 4], [1, 2, 3, 4], [5], [1, 5], [2, 5], [1, 2, 5], [3, 5], [1, 3, 5], [2, 3, 5], [1, 2, 3, 5], [4, 5], [1, 4, 5], [2, 4, 5], [1, 2, 4, 5], [3, 4, 5], [1, 3, 4, 5], [2, 3, 4, 5], [1, 2, 3, 4, 5]]


def all_subsets(q):
    return get_subsets(q, [])


def get_subsets(q, result):
    if len(q) == 0:
        return [[]] + result

    subset = [[q[0]]]
    for r in result:
        subset.append(r[:] + [q[0]])

    [result.append(s) for s in subset]

    return get_subsets(q[1:], result)


print(all_subsets([1, 2, 3, 4, 5]))
