{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List

count x y =
  length $ filter (\z -> z == x) y

someFunc :: IO ()
someFunc = do
  text <- T.readFile "hightemp.txt"
  let l = init $ map (T.splitOn (T.pack "\t")) (T.splitOn (T.pack "\n") text)
  let original = map (\x -> x !! 0) l
  let unique = nub $ map (\x -> x !! 0) l
  let result = sortBy (\x y -> compare (count y original) (count x original)) unique
  T.putStr $ T.concat $ map (\x -> T.append x (T.pack "\n"))result
