module Lib
    ( someFunc
    ) where

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text    as T
import qualified Data.Text.IO as T

template x y z =
  T.concat $ map T.pack [show x, "時の", y, "は", show z]

someFunc :: IO ()
someFunc = do
  T.putStrLn $ template 12 "気温" 22.4
