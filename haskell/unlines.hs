import System.Environment (getArgs)

splitLines [] = []
splitLines cs =
  let (pre, suf) = break isNewLine cs
  in pre : case suf of
    ('\r':'\n':rest) -> splitLines rest
    ('\r':rest)      -> splitLines rest
    ('\n':rest)      -> splitLines rest
    _                -> []

isNewLine c = c == '\r' || c == '\n'

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

fixLines input = unlines (splitLines input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input, output] -> interactWith function input output
            _ -> putStrLn "error: two arguments needed"

        myFunction = fixLines
