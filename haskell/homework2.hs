{-# OPTIONS -XTypeSynonymInstances #-} 

import Test.QuickCheck 

data Tree a = Leaf a | Node (Tree a) (Tree a) 
  deriving (Show)

reduce :: (a->a->a) -> Tree a -> a 
reduce f (Leaf l) = l
reduce f (Node l r) = f (reduce f l) (reduce f r)

toList :: Tree a -> [a] 
toList (Leaf l) = [l]
toList (Node l r) = toList l ++ toList r

reduceList :: (a->a->a) -> [a] -> a 
reduceList f (x : []) = x
reduceList f (x : xs) = (f x (reduceList f xs))

prop_reduceTest :: (Int->Int->Int) -> TS -> Bool 
prop_reduceTest f tree = (reduce f tree) == (reduceList f (toList tree))

type TS = Tree Int 
instance Arbitrary TS where 
  arbitrary = do 
    n <- choose (1,2) :: Gen Int 
    case n of 
      1 -> do i <- arbitrary 
              return (Leaf i) 
      2 -> do t1 <- arbitrary 
              t2 <- arbitrary 
              return (Node t1 t2) 

main = do
  quickCheck (prop_reduceTest (+))
  quickCheck (prop_reduceTest (*))

-- dumpTree x = do
  -- print $ x
  -- print $ toList x
  -- print $ (reduceList (+) (toList x))
  -- print $ (reduce (+) (x))
-- main = do
-- dumpTree $ Node (Node (Leaf 3) (Leaf 6)) (Leaf 5)
