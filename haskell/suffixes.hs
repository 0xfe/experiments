import Data.List

suffixes (x:xs) = (x:xs) : suffixes xs
suffixes [] = []

suffixes2 = init . tails

suffixes3 xs@(x:xs') = xs : suffixes xs'
suffixes3 _ = []

main = do
  print $ suffixes "foobar"
  print $ suffixes2 "foobar"
  print $ suffixes3 "foobar"
