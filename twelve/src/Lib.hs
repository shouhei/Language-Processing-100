{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import qualified Data.Text as T
import qualified Data.Text.IO as T

someFunc :: IO ()
someFunc = do
  text <- readFile "hightemp.txt"
  T.putStrLn $ T.replace (T.pack "\t") (T.pack " ") (T.pack text)
