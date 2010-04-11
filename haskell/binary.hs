import qualified Data.ByteString.Lazy.Char8 as L8
import Control.Monad
import Control.Applicative
import Data.Char

hello :: L8.ByteString
hello = L8.pack "Hello World"

ordify :: L8.ByteString -> [Int]
ordify s = map ord (L8.unpack s)

main = do
  L8.putStrLn hello
  print hello
  print $ ordify hello

  -- <$> = fmap
  ordify <$> (L8.readFile "binaryfile") >>= print

  -- same as above
  (L8.readFile "binaryfile") >>= (print . ordify)
