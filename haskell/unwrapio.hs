-- You can't unwrap an IO because it's a data constructor that's not
-- in scope.

somestring = return "ABC"

unwrap (IO a) = a

stuff = unwrap somestring

main = do
  putStrLn stuff
