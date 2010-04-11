import Data.Maybe

main = do
  print $ Just "just"
  print $ fromMaybe "Nothing" $ Just "just"
