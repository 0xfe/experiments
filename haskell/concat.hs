concatS:: String -> String -> String 
concatS s1 s2 = s1 ++ s2 

showI :: Int -> String 
showI i = show i 

-- g :: String -> Int -> String
g n s = concatS (showI n) s 

-- foldright :: (String -> Int -> String) -> String -> [Int] -> String
foldright h y [] = y 
foldright h y (x:xs) = h x (foldright h y xs)

-- f :: [Int] -> String
f l = foldright g "" l 

main = do
  print $ f [1,4,5]
