{-# LANGUAGE OverloadedStrings, QuasiQuotes, FlexibleContexts, DeriveGeneric, ScopedTypeVariables #-}
module Lib
    ( someFunc
    ) where

import Data.Maybe
import Text.MeCab
import qualified Data.Map as Map
import Text.Regex.PCRE.Heavy

printEach [] = return ()
printEach (x:xs) = do
  putStrLn $ fromJust $ Map.lookup "surface" x
  printEach xs

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
    printEach $ filter (\x -> "動詞" == fromJust (Map.lookup "pos" x)) l
