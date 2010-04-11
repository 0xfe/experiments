{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

-- Prime list. This code is used to test different prime number generators. It
-- returns the number of primes it found that is less than the provided
-- argument, e.g.,
--
--  $ ./primes 1000
--  168
--
-- To build optimally:
--
--  $ ghc --make -O2 primes.hs
import Data.Array
import Data.List
import GHC.Prim
import System (getArgs)

divides :: Integral a => a -> a -> Bool
divides n m = n `mod` m == 0

primes1 :: [Integer]
primes1 = 2 : 3 : 5 : filter isPrime [7..]
  where isPrime = not . hasFactor
        hasFactor n = any ((== 0) . (n `mod`)) $
                        takeWhile (<= (ceiling $ sqrt $ fromInteger n)) primes1

primes' :: [Integer]
primes' = 2 : 3 : 5 : filter (not . hasFactor) [7..]
  where hasFactor n = any (divides n) $ takeWhile (<= lowestFactor n) primes'
        lowestFactor = ceiling . sqrt . fromIntegral

primes3 :: [Integer]
primes3 = 2 : 3 : 5 : filter (not . hasFactor) [7..]
  where hasFactor n = any (divides n) $ takeWhile (<= lowestFactor n) primes3
        lowestFactor = ceiling . sqrt . fromIntegral

-- Primes using sieve of Eratosthenes
sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p > 0]

primes4 :: [Integer]
primes4 = sieve [2..]

-- Like prime3 but using Ints (way faster). But you really don't need to
-- do this. Use prime6, and refer to it at the callsite as (primes6 :: [Int])
primes5 :: [Int]
primes5 = 2 : 3 : 5 : filter (not . hasFactor) [7..]
  where hasFactor n = any (divides n) $ takeWhile (<= lowestFactor n) primes5
        lowestFactor = ceiling . sqrt . fromIntegral

-- Make system generate evens. Note the fact that the source list below is
-- a list of odd numbers. This is the fastest one yet. Also add strictness.
primes6 :: Integral a => [a]
primes6 = 2 : 3 : 5 : filter (not . hasFactor) [7,9..]
  where hasFactor !n = any (divides n) $ takeWhile (<= lowestFactor n) $
                       tail primes6
        lowestFactor = ceiling . sqrt . fromIntegral

-- Dagitses prime. (Note that the 'divides' function here is flipped).
primes7 :: Integral a => [a]
primes7 = 2 : filter isPrime' [3, 5..]
    where isPrime' x = not $ any (divides x) $ possibleDivisors x
          maxFactor = floor . sqrt . fromIntegral
          possibleDivisors x = takeWhile (<= maxFactor x) $ primes7

-- Make system generate evens. Note the fact that the source list below is
-- a list of odd numbers. This is the fastest one yet. Also add strictness.
primes8 :: [Int#]
primes8 = 2 : 3 : 5 : filter (not . hasFactor) [7,9..]
  where hasFactor !n = any (divides n) $ takeWhile (<= lowestFactor n) $
                       tail primes8
        lowestFactor = ceiling . sqrt . fromIntegral

-- This should always point to the fastest prime number generator, but doesn't
-- always seem to be the case.
primes = primes6

main :: IO ()
main = do
  a <- getArgs
  print $ length $ takeWhile (< (read $ a !! 0)) (primes :: [Int#])
