module Main where

import Data.List

type PictureHeader = String
type PictureBody = [Int]
type Checksum = Int

data Picture = Picture {
                    pic_header :: PictureHeader,
                    pic_body :: PictureBody,
                    pic_checksum :: Checksum
                  } deriving Show

validPic p =
  case pic_header p of
    "" -> Nothing
    _ -> Just "Valid"

fromMaybe defval wrapped =
  case wrapped of
    Nothing -> defval
    Just value -> value

main = do
  let p = Picture "Hello" [1,2,3] 5
  let q = Picture "" [1, 2, 3] 6

  print p
  print q

  putStrLn $ "Picture header: " ++ pic_header (p)

  putStrLn $ "Valid: " ++ (fromMaybe "Invalid" $ validPic (p))
  putStrLn $ "Valid: " ++ (fromMaybe "Invalid" $ validPic (q))
