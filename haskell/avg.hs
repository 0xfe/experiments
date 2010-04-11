-- Custom average function calculates the length and sum in
-- a single iteration.

import System (getArgs)

avg (x:xs) (sum,len) = avg xs ((x + sum), (len + 1))
avg [] (sum,len) = sum / (fromIntegral len)

average xs = avg xs (0,0)

main = do
  args <- getArgs

  if length args > 0
    then putStrLn $ show $ average [1 .. (read $ args!!0)]
    else putStrLn "Usage: avg <num>"
