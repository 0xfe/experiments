-- Project Euler: Problem 1
-- Author: Mohit Muthanna Cheppudira
--
-- If we list all the natural numbers below 10 that are multiples of 3 or 5,
-- we get 3, 5, 6 and 9. The sum of these multiples is 23.
--
-- Find the sum of all the multiples of 3 or 5 below 1000.

import System (getArgs)

sum35 :: Int -> Int
sum35 n = sum $ filter multiple35 $ [1..n-1]
  where multiple35 a = any ((== 0) . (a `mod`)) [3, 5]

main :: IO ()
main = do
  a <- getArgs
  print $ sum35 $ if length a == 0 then 1000 else read $ a !! 0
