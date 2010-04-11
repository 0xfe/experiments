import Data.Maybe
import Debug.Trace
import List (sort)

-- Linear time median finder O(2N)

kThSmallest :: (Integral a) => a -> [a] -> Maybe a
kThSmallest 0 _ = Nothing
kThSmallest k [] = Nothing
kThSmallest 1 (xs) = Just $ minimum xs
kThSmallest k xs =
  if k <= (num + lslen)
    then if k > lslen
      then Just pivot
      else kThSmallest k ls
    else kThSmallest (k - (lslen + num)) hs
  where
    (ls, hs, num) = partition (pivot) xs
    pivot = head xs
    minimum (x:xs) = foldr min x xs
    lslen = length ls

partition :: Integer -> [Integer] -> ([Integer], [Integer], Integer)
partition p xs = partition' p [] [] xs 0
  where partition' p ls hs [] num = (ls, hs, num)
        partition' p ls hs (x:xs) num = if x > p
          then partition' p ls (x:hs) xs num
          else if x == p
            then partition' p (ls) hs xs (num + 1)
            else partition' p (x:ls) hs xs num

median :: [Integer] -> Maybe Integer
median [] = Nothing
median xs = kThSmallest (toInteger $ ceiling
  ((fromInteger $ toInteger $ length xs) / 2.0)) xs

main :: IO()
main = do
  print $ sort [1,9,2,3,8,5,2,3,7,5,7,2,7,6,8,4]
  print $ median [1,9,2,3,8,5,2,3,7,5,7,2,7,6,8,4]
  print $ sort [7,9,2,3,8,6,2,3,7,5,7,2,7,6,8,12]
  print $ median [7,9,2,3,8,6,2,3,7,5,7,2,7,6,8,12]
