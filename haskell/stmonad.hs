import Control.Monad.ST.Lazy
import Data.STRef.Lazy

for (a, n) f = if a > n then return ()
               else do
                  f a
                  for (a + 1, n) f

showA n = runST $ do
  r <- newSTRef 0
  for (1, n) (\x -> do
    val <- readSTRef r
    writeSTRef r (val + 1)
    )
  readSTRef r

main = do print $ showA 5
