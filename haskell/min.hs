mymin :: (Ord a) => a -> a -> a
mymin a b = if a < b then a
                     else b

mymax :: (Ord a) => a -> a -> a
mymax a b = if a > b then a
                     else b

minarr :: (Ord a) => [a] -> Maybe a
minarr [] = Nothing
minarr xs = Just $ helper xs
  where helper (x : []) = x
        helper (x : xs) = mymin x (helper xs)

minarr2 :: (Ord a) => [a] -> Maybe a
minarr2 [] = Nothing
minarr2 (x : xs) = Just $ foldr mymin x xs

fromMaybe :: a -> Maybe a -> a
fromMaybe defval Nothing = defval
fromMaybe defval (Just a) = a

main = do
  print $ mymin 4 5
  print $ mymax 4 5
  print $ fromMaybe (-1) (minarr [12, 4, 5, 76, 3, 4, 4, 8, 43])
  print $ fromMaybe (-1) (minarr2 [12, 4, 5, 76, 3, 4, 4, 8, 43])
  print $ fromMaybe (-1) (minarr2 [])
