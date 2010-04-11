import Debug.Trace

-- A neat debugging trick. Use "trace" in a guard that always fails
-- to log a message every time the function is evaluated.

f :: Int -> Int
f n | trace ("f was called: " ++ show n) False = undefined
f 0 = 1
f n = n * f (n - 1)


main = do
  print $ f 7
