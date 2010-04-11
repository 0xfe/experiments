module Main
  where

import IO
import Random

doGuessing num = do
  putStrLn "Your guess?"
  guess <- getLine
  let guessNum = read guess
  if guessNum < num
    then do putStrLn "Too low!"
            doGuessing num
    else if guessNum > num
            then do putStrLn "Too high"
                    doGuessing num
            else do putStrLn "Correct!"

main = do
  hSetBuffering stdin LineBuffering
  num <- randomRIO(1::Int, 100)
  putStrLn "I'm thinking of a number between 1 and 100"
  doGuessing num
