module Lib
    ( someFunc
    ) where

import Data.List

makeNgram :: Int -> [a] -> [[a]]
makeNgram n m
  | n >= length m = m : []
  | otherwise     = (take n m) : (makeNgram n (tail m))

someFunc :: IO ()
someFunc = do
  let x = nub $ makeNgram 2 "paraparaparadise"
  let y = nub $ makeNgram 2 "paragraph"
  putStr "join: "
  print $ union x y
  putStr "diff: "
  print $ x \\ y
  putStr "prod: "
  print $ intersect x y
  putStr "has se: "
  let result = elemIndex "se" $ union x y
  print $ case result of
    Just _ -> "yes"
    Nothing -> "no"
