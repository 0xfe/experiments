module Main
  where

import IO
import Random

x = 5
y = (6, "hello")
z = x * fst y

addthem x y = x + y

run_this = do
  putStrLn ("A")
  putStrLn ("B")

getNumbers = do
  putStrLn "enter a number: "
  word <- getLine

  if read word == 0
    then return []
    else do
      rest <- getNumbers
      return ((read word :: Int ): rest)

showNumbers [] = return ()
showNumbers (x:xs) = do
  putStrLn (show x)
  showNumbers xs

my_min :: [a] -> Maybe a
my_min [] = Nothing
my_min (x:xs) = do
  min <- read my_min xs
  if min < x
    then Just min
    else Just x
  

main = do
  numbers <- getNumbers
  showNumbers numbers
  putStrLn ("Minimum: ")

  min <- my_min numbers
  putStrLn (show min)
