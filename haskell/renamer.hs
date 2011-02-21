-- Requires PCRE
--
--  cabal install regex-pcre

import Control.Applicative
import Control.Monad
import Data.List
import System
import System.Posix.Directory
import System.Posix.Files
import Text.Regex.PCRE

ls :: String -> IO ExitCode
ls params = system $ "ls " ++ params

getDirEntries :: FilePath -> IO [FilePath]
getDirEntries dir = openDirStream dir >>= start
  where start s = readDirStream s >>= rest s
        rest s "" = return []
        rest s entry = liftM (entry:) (start s)

main :: IO ()
main = do
  if length args < 2
    then putStrLn "Usage: renamer dir match"
    else do
      files <- getDirEntries $ args !! 0
      let matches = filter (=~ (args !! 1)) files
      print matches
