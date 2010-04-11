-- Cross platfrom line-splitting.
-- Better than 'lines'

splitLines [] = []
splitLines cs =
  let (pre, suf) = break isNewLine cs
  in pre : case suf of
    ('\r':'\n':rest) -> splitLines rest
    ('\r':rest)      -> splitLines rest
    ('\n':rest)      -> splitLines rest
    _                -> []

isNewLine c = c == '\r' || c == '\n'

main = do
  print $ splitLines "Hello\nHow\rAre\r\nYou?"

