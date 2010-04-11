powerSet' = foldr (\x -> concatMap (\xs -> [x:xs,xs])) [[]]

main = do
  print $ powerSet' ['m', 'o', 'h', 'i', 't']
