-- Usage: hexdump <filename>

import Control.Monad (when)
import Data.Char
import Data.List
import Data.Maybe
import Numeric
import qualified Data.ByteString.Lazy as L
import System.Exit
import System (getArgs)

toHex' c = [upperHex c, lowerHex c]
  where upperHex = toDigit . (`div` 16)
        lowerHex = toDigit . (`mod` 16)
        toDigit = intToDigit . fromIntegral

toHex c = showIntAtBase 16 intToDigit c ""
toBinary c = showIntAtBase 2 intToDigit c ""

hexString :: L.ByteString -> String
hexString content = intercalate ", " (map toHex (L.unpack content))

fileHexDump :: FilePath -> IO String
fileHexDump path = do
  content <- L.readFile path
  return $ hexString content

main = do
  args <- getArgs

  when (length args /= 1) $ do
    putStrLn "Syntax: hexdump <filename>"
    exitFailure

  hex_text <- fileHexDump $ args !! 0
  putStrLn $ hex_text
