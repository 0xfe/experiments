-- Project Euler: Problem 10
-- Author: Mohit Muthanna Cheppudira
--
-- Calculate the sum of all the primes below two million.

import System (getArgs)

-- Memoized prime number generator.
primes :: Integral a => [a]
primes = 3 : 5 : filter (not . hasFactor) [7,9..]
  where hasFactor n = any (divides n) $ takeWhile (<= lowestFactor n) primes
        divides n m = n `mod` m == 0
        lowestFactor = ceiling . sqrt . fromIntegral

euler10 :: Int -> Int
euler10 limit = sum $ takeWhile (< limit) (primes :: [Int])

main :: IO ()
main = do
  a <- getArgs
  print $ euler10 $ defaultArg a 2000000
    where defaultArg a b = if length a == 0 then b else read $ a !! 0
