-- Project Euler: Problem 3
-- Author: Mohit Muthanna Cheppudira
--
-- The prime factors of 13195 are 5, 7, 13 and 29.
-- 
-- What is the largest prime factor of the number 600851475143 ?

import System (getArgs)

-- Memoized prime number generator.
primes :: [Integer]
primes = 2 : 3 : 5 : filter (not . hasFactor) [7..]
  where hasFactor n = any (divides n) $ takeWhile (<= lowestFactor n) primes
        divides n m = n `mod` m == 0
        lowestFactor n = ceiling $ sqrt $ fromInteger n

-- Given an integer, generate list of prime factors.
primeFactors :: Integer -> [Integer]
primeFactors n = takeUntil allFactorsFound [] $ filter isFactor primes
  where takeUntil d acc (x:xs) =
          if d (x:acc) then (x:acc) else takeUntil d (x:acc) xs
        allFactorsFound acc = product acc == n
        isFactor m = n `mod` m == 0

main :: IO ()
main = do
  a <- getArgs
  print $ maximum $ primeFactors $ defaultArg a 600851475143
    where defaultArg a b = if length a == 0 then b else read $ a !! 0
