# https://leetcode.com/problems/longest-palindromic-substring/


def longestPalindrome(s: str) -> str:
    if len(s) == 1:
        return s[0]

    if s[0] == s[1]:
        longest = (0, 1)
        max_len = 2
    else:
        longest = (0, 0)
        max_len = 1

    for center in range(1, len(s)):
        for shift in (0, 1):
            lp = center
            rp = center + shift

            while lp >= 0 and rp < len(s):
                if s[lp] != s[rp]:
                    break

                cur_len = (rp - lp) + 1
                if cur_len > max_len:
                    max_len = cur_len
                    longest = (lp, rp)

                lp -= 1
                rp += 1

            lp = center
            rp = center + 1

    return s[longest[0] : longest[1] + 1]


def test_longestPalindrome():
    assert longestPalindrome("babad") == "bab"
    assert longestPalindrome("cbbd") == "bb"
    assert longestPalindrome("a") == "a"
    assert longestPalindrome("bb") == "bb"
    assert longestPalindrome("bbbb") == "bbbb"
    assert longestPalindrome("bbbbb") == "bbbbb"
    assert longestPalindrome("ababasdfnkasjffffff") == "ffffff"
    assert longestPalindrome("aacabdkacaa") == "aca"


test_longestPalindrome()
