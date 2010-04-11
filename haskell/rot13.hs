module Main (main) where

import IO
import Char
import System(getArgs)
import System.Console.GetOpt

rot13Char x | (ord 'a' <= ord(toLower x)) &&
              (ord(toLower x) <= ord 'm') = chr(ord x + 13)
            | (ord 'n' <= ord(toLower x)) &&
              (ord(toLower x) <= ord 'z') = chr(ord x - 13)
            | otherwise = x

rot13 :: [Flag] -> String -> String
rot13 flags contents = map rot13Char contents

showHelp :: IO ()
showHelp = do
  putStrLn "Rot13.hs 1.1 - Mohit Cheppudira"
  putStrLn "Usage:"
  putStrLn "  rot13 filename [filename ...]"

showError :: String -> IO ()
showError msg = do
  putStrLn msg
  showHelp

startRot13 args =
  case getOpt RequireOrder options (init args) of
    (flags, nonOpts, [])  -> case nonOpts of
        [] -> do
          contents <- readFile (last args)
          putStrLn $ rot13 flags contents
        _ -> showError $ "Unrecognized arguments: " ++ unwords nonOpts
    (_, _, msgs)     -> showError $ concat msgs

main :: IO ()
main = do
  args <- getArgs
  if length args == 0
    then showHelp
    else startRot13 args

data Flag = Help

options :: [OptDescr Flag]
options = [ Option ['h'] ["help"] (NoArg Help) "show this help message" ]
