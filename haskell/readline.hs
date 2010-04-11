-- Test the readline library, with history recall.

module Main (main) where

import IO
import Maybe
import System.Console.Readline

getline :: IO String
getline = do
  line <- readline "> "
  let parsed_line = fromJust line
  addHistory parsed_line
  return $ parsed_line

main :: IO ()
main = do
  line <- getline
  if line == "q"
     then return ()
     else main
