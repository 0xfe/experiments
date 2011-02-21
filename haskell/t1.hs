import Data.Char

data Tree a = TreeNode (Tree a) (Tree a) a | LeafNode a
  deriving (Show)

process = map toUpper

tree = TreeNode (LeafNode "a") (LeafNode "b") "r"

main = do
  putStrLn "What's your name, bozo?"
  s <- getLine
  putStrLn $ "Hello " ++ (process s) ++ "!"
  putStrLn $ show tree
