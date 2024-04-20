# https://leetcode.com/problems/minimum-window-substring/


def trace(f):
    def wrapper(*args, **kwargs):
        print(f"{f.__name__} {' '.join(args)} {kwargs}")
        ans = f(*args, **kwargs)
        print(" -->", ans)
        return ans

    return wrapper


@trace
def minWindow(s: str, t: str) -> str:
    lp = 0
    rp = 0
    min_window = (0, 0)
    target = len(t)
    char_map = {}

    for c in t:
        if c in char_map:
            char_map[c] += 1
        else:
            char_map[c] = 1

    while rp < len(s):
        if s[rp] in char_map and char_map[rp] > 0:
            char_map[rp] -= 1
            target -= 1
        else:
            lp += 1
            if s[lp] in char_map:
                char_map[lp] += 1
                target += 1


assert minWindow("ADOBECODEBANC", "ABC") == "BANC"
