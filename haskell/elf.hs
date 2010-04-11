-- Usage: elf <filename>

import qualified Data.ByteString.Lazy as L
import System (getArgs)

hasElfMagic :: L.ByteString -> Bool
hasElfMagic content = L.take 4 content == elfMagic
  where elfMagic = L.pack [0x7f, 0x45, 0x4c, 0x46]

isElfFile :: FilePath -> IO Bool
isElfFile path = do
  content <- L.readFile path
  return $ hasElfMagic content

main = do
  args <- getArgs
  isElf <- isElfFile $ args !! 0
  putStrLn $ if isElf then "ELF" else "NOT ELF"
