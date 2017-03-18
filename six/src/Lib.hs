module Lib
    ( someFunc
    ) where

makeNgram :: Int -> [a] -> [[a]]
makeNgram n m
  | n >= length m = m : []
  | otherwise     = (take n m) : (makeNgram n (tail m))

someFunc :: IO ()
someFunc = do
  print $ makeNgram 2 "I am an NLPer"
  print $ makeNgram 2 (words "I am an NLPer")
