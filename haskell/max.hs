module Main 
  where

import System

type Person = String
type Book = String

type Database = [ (Person, Book) ]

exampleBase :: Database
exampleBase = [ ("Mohit", "Life of Pi"), ("Anna", "Asterix"),
                ("Jon", "Slaves to the Grind"), ("Mohit", "Machine Learning")]

borrow :: Database -> Person -> Book -> Database
borrow d p b = (p, b):d

maxarr :: [Int] -> Int
maxarr (x:xs) | xs == [] = x
              | otherwise = max x (maxarr xs)
maxarr [] = 0

minarr :: [Int] -> Int
minarr (x:xs) 
  | xs == [] = x
  | otherwise = min x (minarr xs)
minarr [] = 0

sumarr :: [Int] -> Int
sumarr [] = 0
sumarr (x:xs) = x + (sumarr xs)

main = do
  args <- getArgs
  let arr = map read args
  print arr
  putStrLn $ "Maximum: " ++ (show $ maxarr arr)
  putStrLn $ "Minimum: " ++ (show $ minarr arr)
  putStrLn $ "Sum: " ++ (show $ sumarr arr)
  print exampleBase
  print $ borrow exampleBase "George" "The World is Flat"
