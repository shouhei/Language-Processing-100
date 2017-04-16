{-# LANGUAGE OverloadedStrings, QuasiQuotes, FlexibleContexts, DeriveGeneric, ScopedTypeVariables #-}
module Lib
    ( someFunc
    ) where

import Data.List
import Data.Maybe
import Text.MeCab
import qualified Data.Map as Map
import Text.Regex.PCRE.Heavy
import Graphics.Gnuplot.Simple

base x = (fromJust (Map.lookup "base" x))

frequency (_:[]) = []
frequency (x:xs) = do
  let tmp = filter (\y -> (base x) == (base y)) xs
  (base x, length(tmp)+1): frequency (filter (\y -> (base x) /= (base y)) xs)

makeMecabMap [] = []
makeMecabMap (x:xs) = do
  let l = split [re|(\t|,)|] x
  let m = Map.fromList [("surface", l !! 0),("base", l !! 7), ("pos", l !! 1), ("pos1", l !! 2)]
  m : makeMecabMap xs

makeFrequencyList :: [(String, Int)] -> [(Int, Int)]
makeFrequencyList [] = []
makeFrequencyList (x:xs) = do
  let num = length $ filter (\y -> (snd x) == (snd y)) xs
  ((snd x), num + 1): (makeFrequencyList (filter (\y -> (snd x) /= (snd y)) xs))

someFunc :: IO ()
someFunc = do
  text <- readFile "neko.txt"
  mecab  <- new2 ""
  result <- parse mecab text
  let l = makeMecabMap $ init $ lines result
  plotPathStyle [(Title "Frequency")] (defaultStyle{plotType = Boxes }) $ sortBy (\x y-> compare (fst y) (fst x)) $ makeFrequencyList (frequency l)
