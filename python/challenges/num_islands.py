# https://leetcode.com/problems/number-of-islands/


def follow(grid, i, j, counter):
    grid[i][j] = counter
    for m in (-1, +1):
        y = i + m
        if y >= 0 and y < len(grid):
            if grid[y][j] == "1":
                follow(grid, y, j, counter)

    for n in (-1, +1):
        x = j + n
        if x >= 0 and x < len(grid[i]):
            if grid[i][x] == "1":
                follow(grid, i, x, counter)


def numIslands(grid: list[list[str]]) -> int:
    counter = 2

    for i in range(len(grid)):
        for j in range(len(grid[i])):
            if grid[i][j] == "1":
                follow(grid, i, j, counter)
                counter += 1

    ans = counter - 2
    return ans


assert (
    numIslands(
        [
            ["1", "1", "1", "1", "0"],
            ["1", "1", "0", "1", "0"],
            ["1", "1", "0", "0", "0"],
            ["0", "0", "0", "0", "0"],
        ],
    )
    == 1
)

assert (
    numIslands(
        [
            ["1", "1", "0", "0", "0"],
            ["1", "1", "0", "0", "0"],
            ["0", "0", "1", "0", "0"],
            ["0", "0", "0", "1", "1"],
        ]
    )
    == 3
)

assert numIslands([["1", "1", "1"], ["0", "1", "0"], ["1", "1", "1"]]) == 1
