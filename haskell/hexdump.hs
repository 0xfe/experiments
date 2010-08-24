-- Usage: hexdump <filename>
-- Author: Mohit Cheppudira <mohit@muthanna.com>

import Control.Monad (when)
import Data.Char
import Data.List
import Numeric
import qualified Data.ByteString.Lazy as L
import System.Exit
import System (getArgs)
import Text.Printf

bytesPerLine = 16

toHex c = showIntAtBase 16 intToDigit c ""
toHexList = (map $ printf "%02s" . toHex) . L.unpack

fileToHex path = do
  content <- L.readFile path
  return $ toHexList content

formatList 0 _ = error "x < 1"
formatList x [] = []
formatList x l = h : formatList x t
  where (h, t) = splitAt x l

showWithAddress [] _ = []
showWithAddress (x:xs) a = prettify : showWithAddress xs (a + bytesPerLine)
  where prettify = (toHex a) ++ " " ++ (intercalate " " x)

showHexFile file = do
    hexData <- fileToHex $ file
    mapM putStrLn $ showWithAddress (formatList bytesPerLine hexData) 0

main = do
  args <- getArgs

  when (length args /= 1) $ do
    putStrLn "Syntax: hexdump <filename>"
    exitFailure

  showHexFile $ args !! 0
