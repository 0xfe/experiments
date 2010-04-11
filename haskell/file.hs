module Main
  where

import IO

file = "file.txt"

process :: String -> Int
process contents =
  let x = read $ take 1 contents in
  x + 1

main = do
  bracket (openFile file ReadMode) hClose
          (\h -> do contents <- hGetContents h
                    putStrLn $ show $ process contents)
  putStrLn "All done!"
