{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import Data.List.Split
import qualified Data.Text as T
import qualified Data.Text.IO as T

someFunc :: IO ()
someFunc = do
  text <- readFile "hightemp.txt"
  let l = (map (splitOn "\t") (filter (\x -> x /= "") (splitOn "\n" text)))
  let col1 = map(\x -> T.pack x) $ map (\x -> x !! 0) l
  let col2 = map(\x -> T.pack x) $ map (\x -> x !! 1) l
  T.writeFile "col1.txt" $ T.concat $ map (\x -> (T.append x (T.pack "\n"))) col1
  T.writeFile "col2.txt" $ T.concat $ map (\x -> (T.append x (T.pack "\n"))) col2
