-- Build logger and state monads from memory.

import Control.Monad

-- Logger monad
type Log = [String]

-- note: 'newtype' is just like 'data' but without the overhead
newtype Logger a = Logger { execLogger :: (a, Log) }

record :: String -> Logger ()
record s = Logger ((), [s])

instance Monad Logger where
  return a = Logger (a, [])

  m >>= k = let (a, l)   = execLogger m
                n        = k a
                (a', l') = execLogger n
            in Logger (a', l ++ l')

logThis :: String -> Logger ()
logThis s = do
  record "Starting logThis"
  record s
  record "Ending logThis"

-- State monad

newtype State s a = State { execState :: s -> (a, s) }

returnSt :: a -> State s a
returnSt a = State $ \s -> (a, s)

-- note: type returns ()
setSt :: s -> State s ()
setSt s = State $ \_ -> ((), s)

getSt :: State s s
getSt = State $ \s -> (s, s)

instance Monad (State s) where
  return = returnSt

  m >>= k = State $ \s -> let (a, s') = execState m s
                          in execState (k a) s'

helloState :: State String ()
helloState = setSt "Hello World"

paramState :: String -> State String ()
paramState s = setSt s

-- Sum up an array using state monad.
sumState :: [Int] -> Int
sumState xs = snd $ execState addup 0 
  where addup = forM_ xs $ \x -> do
                  i <- getSt
                  setSt $ i + x

main :: IO ()
main = do
  let (_, log) = execLogger $ logThis "mmuthanna"
  print log

  print $ snd $ execState (paramState "mmuthanna") ""

  print $ snd (execState helloState "")

  print $ sumState [5, 4, 94, 23, 55]
