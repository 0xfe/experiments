remove_second xs =
  head xs : tail (tail xs)

main = do
  print $ remove_second [1, 2, 3, 4, 5, 6]
