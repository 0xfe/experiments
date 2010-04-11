-- Project Euler: Problem 35
-- Author: Mohit Muthanna Cheppudira
--
-- The number, 197, is called a circular prime because all rotations of the
-- digits: 197, 971, and 719, are themselves prime.
--
-- There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37,
-- 71, 73, 79, and 97.
--
-- How many circular primes are there below one million?

import Data.Set (Set)
import qualified Data.Set as Set
import System (getArgs)

-- How many circular primes do you want?
limit = 1000000

-- Memoized prime number generator.
primes :: [Integer]
primes = 2 : 3 : 5 : filter (not . hasFactor) [7..]
  where hasFactor n = any (divides n) $ takeWhile (<= lowestFactor n) primes
        divides n m = n `mod` m == 0
        lowestFactor = ceiling . sqrt . fromIntegral

-- Given an integer, return the number of digits in it.
digits :: Integer -> Int
digits n = ceiling $ logBase 10 $ fromIntegral (n + 1)

-- Rotate number right:
--   rotate 456 = 645
--   rotate 3498 = 8349
--
-- Note that this does not work for numbers that have zeroes in them. But
-- that's okay because any number with a zero in it is not a circular prime.
-- Also, right rotation catches the zero before it disappears into the
-- significant digit.
rotate :: Integer -> Integer
rotate n = strip_last_digit + (last_digit * tens_place)
  where last_digit = n `mod` 10
        strip_last_digit = (n - last_digit) `div` 10
        tens_place = (10 ^ (digits n - 1))

-- Set of million primes. This enables fast lookup in isCircularPrime.
primeSet :: Set Integer
primeSet = Set.fromList $ takeWhile (< limit) primes

-- Returns True if number is prime.
isPrime :: Integer -> Bool
isPrime n = Set.member n primeSet

-- Returns True if the number is a circular prime.
isCircularPrime :: Integer -> Bool
isCircularPrime n = all isPrime $ take (digits n) $ iterate rotate n

-- Go.
main :: IO ()
main = print $ length $ filter isCircularPrime $ takeWhile (< limit) primes
