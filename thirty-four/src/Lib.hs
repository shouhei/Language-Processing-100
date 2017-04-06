{-# LANGUAGE OverloadedStrings, QuasiQuotes, FlexibleContexts, DeriveGeneric, ScopedTypeVariables #-}
module Lib
    ( someFunc
    ) where

import Data.Maybe
import Text.MeCab
import qualified Data.Map as Map
import Text.Regex.PCRE.Heavy


isNoun x = "名詞" == (fromJust $ Map.lookup "pos" x)

isParticipleAdjective x = "連体化" == (fromJust $ Map.lookup "pos1" x)

getSurface x = (fromJust $ Map.lookup "surface" x)

filterConcatenatedNoun x
  | length x < 3 = do
      return ()
  | (length x) >= 3 && (isNoun (x !! 0)) && (isParticipleAdjective (x !! 1)) && (isNoun (x !! 2)) = do
      putStrLn $ (getSurface (x !! 0)) ++ (getSurface (x !! 1)) ++ (getSurface (x !! 2)) ++ "\NUL"
      filterConcatenatedNoun (tail x)
  | otherwise = do
      filterConcatenatedNoun (tail x)


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
    filterConcatenatedNoun l
