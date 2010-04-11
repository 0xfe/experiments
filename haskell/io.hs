-- times :: Integer -> a -> m ()
times 0 f = return ()
times x f = do
  f
  times (x - 1) f

-- This works because the type of main is IO t
main = do
  putStrLn "How many more times?"
  inpStr <- getLine
  times (read inpStr) (putStrLn "Hello") 

