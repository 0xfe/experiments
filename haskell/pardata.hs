{-# LANGUAGE PArr #-}

-- Testing Parallel Arrays
-- $ ghc --make -threaded -fdph-par -fforce-recomp pardata.hs
-- $ ./pardata +RTS -N2

import Data.Array.Parallel.Prelude
import qualified Data.Array.Parallel.Prelude.Double as D

sumSq :: [: Double :] -> Double
sumSq a = D.sumP (mapP (\x -> x * x) a)

main :: IO ()
main = do
  let sum = sumSq [: 4, 5, 6, 3.2, 5.3, 45.2 :]
  putStrLn $ show sum
