def fib_1(num):
    if num < 0:
        raise ValueError(num)
    if num == 0:
        return 0

    if num == 1 or num == 2:
        return 1

    return fib_1(num - 1) + fib_1(num - 2)


def fib_2(num):
    t = {}
    t[0] = 0
    t[1] = 1
    t[2] = 1

    if num < 0:
        raise ValueError(num)

    if num < 2:
        return t[num]

    for n in range(3, num + 1):
        t[n] = t[n - 1] + t[n - 2]

    return t[num]


print(5, fib_2(50))
print(5, fib_1(50))
