{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import Data.List
import Data.List.Split
import qualified Data.Text as T
import qualified Data.Text.IO as T

printEach [] = return ()
printEach (x:xs) = do
  T.putStrLn x
  printEach xs

someFunc :: IO ()
someFunc = do
  text <- readFile "hightemp.txt"
  let l = (map (splitOn "\t") (filter (\x -> x /= "") (splitOn "\n" text)))
  let col1 = nub $ sort $ map(\x -> T.pack x) $ map (\x -> x !! 0) l
  printEach col1
