{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List

printEachLine [] = do return ()
printEachLine (x:xs) = do
  T.putStrLn $ T.concat $ map (\y -> T.append y (T.pack("\t"))) x
  printEachLine xs

someFunc :: IO ()
someFunc = do
  text <- T.readFile "hightemp.txt"
  let l = filter (\x -> x /= "") (T.splitOn (T.pack "\n") text)
  let s = sortBy (\x y -> compare (x !! 2) (y !! 2)) (map (T.splitOn (T.pack "\t")) l)
  printEachLine s

