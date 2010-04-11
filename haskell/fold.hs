main = do
  -- This won't compile.
  -- print $ foldl (:) [] [1..10] -- Does not compile, obviously!
  print $ foldr (:) [] [1..10]

  -- These work
  print $ foldl (+) 0 [1..10]
  print $ foldr (+) 0 [1..10]
