data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show)

-- Take a tree, and return a new tree with the leaf count attached
-- to each leaf. The leaves are counted left to right.

numberLeaves :: Tree a -> Tree (a, Int)
numberLeaves t = fst(helper t 1)
  where
    helper (Leaf l) i = (Leaf (l, i), i + 1)
    helper (Node l r) i = ((Node (lnode)) (rnode), ri)
      where lhelper = helper l i
            rhelper = helper r li
            lnode = fst(lhelper)
            li = snd(lhelper)
            rnode = fst(rhelper)
            ri = snd(rhelper)

genTree = Node
            (Leaf "X")
            (Node
              (Node (Leaf "A")
                    (Leaf "B"))
              (Node (Leaf "C")
                    (Leaf "D")))

newtype State s a = State { runState :: s -> (a, s) }

returnState :: a -> State s a
returnState a = State (\s -> (a, s))

bindState :: State s a -> (a -> State s b) -> State s b
bindState m k = State (
  \oldState ->
  let (a, s') = runState m oldState
  in runState (k a) s')

instance Monad (State s) where
  return = returnState
  (>>=) = bindState

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put s = State (\_ -> ((), s))

update :: (s -> s) -> State s s
update f = State (\s -> (s, f s))

numberLeavesM :: Tree a -> Tree (a, Int)
numberLeavesM t = fst(runState (number t) 0)
  where
    number (Leaf i) = do
      x <- get
      put(x + 1)
      returnState (Leaf (i, x + 1))
    number (Node l r) = do
      l' <- number l
      r' <- number r
      returnState (Node l' r')

main = do
  putStrLn ""
  putStrLn "No monad:"
  print $ numberLeaves $ genTree
  putStrLn ""
  putStrLn "With monad:"
  print $ numberLeavesM $ genTree
