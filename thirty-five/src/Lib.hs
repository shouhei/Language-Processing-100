{-# LANGUAGE OverloadedStrings, QuasiQuotes, FlexibleContexts, DeriveGeneric, ScopedTypeVariables #-}
module Lib
    ( someFunc
    ) where

import Data.Maybe
import Text.MeCab
import qualified Data.Map as Map
import Text.Regex.PCRE.Heavy

maxLength x y
  | (length x) > y = do
    let n = length $ takeWhile (\z -> "名詞" == fromJust (Map.lookup "base" z)) x
    let c = (max n 1)
    maxLength (drop c x) (max n y)
  | otherwise = y

makeMecabMap [] = [Map.fromList [("surface", ""), ("base", ""), ("pos", ""), ("pos1", "")]]
makeMecabMap (x:xs) = do
  let l = split [re|(\t|,)|] x
  let m = Map.fromList [("surface", l !! 0),("base", l !! 7), ("pos", l !! 1), ("pos1", l !! 2)]
  m : makeMecabMap xs

someFunc :: IO ()
someFunc = do
    text <- readFile "neko.txt"
    mecab  <- new2 ""
    result <- parse mecab text
    let l = makeMecabMap $ init $ lines result
    print $ maxLength l 0
