-- Build a logger monad, and use it in an operation

type Log = [String]

newtype Logger a = Logger { execLogger :: (a, Log) }

-- Here runLogger is aliased to execLogger. This works because
-- execLogger is a function that takes a Logger and returns the
-- associated data element: (a, Log).
runLogger :: Logger a -> (a, Log)
runLogger = execLogger

-- Return nothing, just log the string.
record s = Logger ((), [s])

instance Monad Logger where
  -- Return a, log nothing
  return a = Logger (a, [])

  -- The magic happens here. Execute m then k, and concatenate
  -- the associated log message array.
  m >>= k = let (a, w) = execLogger m
                n      = k a 
                (b, x) = execLogger n
            in Logger (b, w ++ x)


main = do print $ runLogger $
            do record "Hello World!"
               return 3.1337
