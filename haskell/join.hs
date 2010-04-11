join :: String -> [String] -> String
join s (x : []) = x
join s (x : xs) = x ++ s ++ (join s xs)
join s [] = []


main = do
  print $ join ", " ["a", "b", "c"]
