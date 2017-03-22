module Lib
    ( someFunc
    ) where

import Data.List.Split

someFunc :: IO ()
someFunc = do
  text <- readFile "hightemp.txt"
  print $ length $ filter (\x -> x /= "") (splitOn "\n" text)
