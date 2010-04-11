import System.Environment (getArgs)
import Data.Char

upperify = map toUpper

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input, output] -> interactWith function input output
            _ -> putStrLn "error: two arguments needed"

        myFunction = upperify
