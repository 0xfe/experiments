-- quick and dirty super simple quicksort functions

sort (x:xs) = lesser ++ x:greater
  where lesser  = sort $ filter (<x) xs
        greater = sort $ filter (>x) xs
sort _      = []

sort1 (x:xs) = (pivot (<x)) ++ x : pivot (>x)
  where pivot f  = sort1 $ filter f xs
sort1 _      = []

sort2 (x:xs) = sort2 [y | y <-xs, y < x] ++ x : sort2 [y | y <-xs, y > x]
sort2 _      = []

sort3 (x:xs) = (sort3 $ filter (<x) xs) ++ x : (sort3 $ filter (>x) xs)
sort3 _      = []

myarray = [3,4,6,2,4,1,39,4,48,3,43,23,18,04]

main = do print $ sort  myarray
          print $ sort1 myarray
          print $ sort2 myarray
          print $ sort3 myarray
