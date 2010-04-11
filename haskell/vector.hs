data Vector a = Vector a a a
  deriving (Show)

sumof :: (Num t) => Vector t -> t
sumof (Vector a b c) = a + b + c

main :: IO ()
main = do
  print $ sumof (Vector 1 2 3)
  print $ "Is all"
