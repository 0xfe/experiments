-- Build a simple bitmap using STUArray

import Control.Applicative
import Control.Monad
import Control.Monad.ST (ST)
import Data.Word (Word32)
import Data.Array.MArray
import Data.Array.IArray
import Data.Array.ST (STUArray, runSTUArray)
import Prelude hiding (length)

-- Define BitArray:
--   s: State
--   Word32: Index type
--   Bool: Array content type
data BitArray s = BitArray (STUArray s Word32 Bool)

-- Create a BitArray
newBitArray :: Word32 -> Bool -> ST s (BitArray s)
newBitArray numBits defaultBit =
  BitArray `liftM` newArray (0, numBits - 1) defaultBit

-- Extract the STUArray from the BitArray
getArray :: BitArray s -> STUArray s Word32 Bool
getArray (BitArray a) = a

length :: BitArray s -> ST s Word32
length (BitArray a) = (succ . snd) `liftM` getBounds a

insert :: BitArray s -> Word32 -> Bool -> ST s ()
insert (BitArray a) index elem = writeArray a index elem

getValue :: BitArray s -> Word32 -> ST s Bool
getValue (BitArray a) index = readArray a index

main :: IO ()
main = do
  let arr =
        runSTUArray $ do
        arr <- newBitArray 10 False
        len <- length arr
        insert arr 4 True
        four <- getValue arr 4
        five <- getValue arr 5
        return $ getArray arr

  -- runSTUArray returns a UArray (which is in arr here)
  print arr

  -- UArray is an instance of IArray, that defines (!)
  putStrLn $ "Fourth element: " ++ (show $ arr ! 4)
