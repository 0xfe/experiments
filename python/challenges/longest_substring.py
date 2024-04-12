def lengthOfLongestSubstring(s: str) -> int:
    if len(s) < 2:
        return len(s)

    lp = 0
    rp = 0
    current_len = 0
    longest = (0, 0, 0)  # len, l, r
    chars = {}

    while rp < len(s):
        print(
            f"lp: {lp}, rp: {rp}, current_len: {current_len}, longest: {longest}, chars: {chars}"
        )
        rc = s[rp]

        if rc in chars and chars[rc] >= lp:
            lp = chars[rc] + 1
            current_len = rp - lp
        else:
            chars[rc] = rp
            rp += 1
            current_len += 1
            if current_len > longest[0]:
                longest = (current_len, lp, rp)

    return longest[0]


def test_lengthOfLongestSubstring():
    assert lengthOfLongestSubstring("abcabcbb") == 3
    assert lengthOfLongestSubstring("bbbbb") == 1
    assert lengthOfLongestSubstring("pwwkew") == 3
    assert lengthOfLongestSubstring("") == 0
    assert lengthOfLongestSubstring(" ") == 1
    assert lengthOfLongestSubstring("au") == 2
    assert lengthOfLongestSubstring("dvdf") == 3
    assert lengthOfLongestSubstring("tmmzuxt") == 5
    assert lengthOfLongestSubstring("abba") == 2
    assert lengthOfLongestSubstring("aab") == 2
    assert lengthOfLongestSubstring("aabaab!bb") == 3


test_lengthOfLongestSubstring()
