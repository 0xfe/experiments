{-# LANGUAGE FlexibleInstances #-}

y f = f $ y ( f )

factRec g x = case x of
                0 -> 1
                _ -> x * (g (x - 1))

fact = y factRec

fibRec g x = case x of
                0 -> 0
                1 -> 1
                _ -> (g (x - 1) + g (x - 2))

fib = y fibRec

reduceRec g f l = case l of
                    [] -> undefined
                    [x] -> x
                    (x : xs) -> f x (g f xs)

reduce = y reduceRec
