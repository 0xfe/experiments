-- Project Euler: Problem 7
-- Author: Mohit Muthanna Cheppudira
--
-- Find the 10001st Prime

import System (getArgs)

-- Memoized prime number generator.
primes :: [Int]
primes = 2 : 3 : 5 : filter (not . hasFactor) [7..]
  where hasFactor n = any (divides n) $ takeWhile (<= lowestFactor n) primes
        divides n m = n `mod` m == 0
        lowestFactor = ceiling . sqrt . fromIntegral

main :: IO ()
main = do
  a <- getArgs
  print $ primes !! ((defaultArg a 10001) - 1)
    where defaultArg a b = if length a == 0 then b else read $ a !! 0
