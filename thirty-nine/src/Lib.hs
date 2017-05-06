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

{--
基本形を素に、出現頻度を作成する
[a] -> [(基本形, 頻度)]
--}
frequency (_:[]) = []
frequency (x:xs) = do
  let tmp = filter (\y -> (base x) == (base y)) xs
  (base x, length(tmp)+1): frequency (filter (\y -> (base x) /= (base y)) xs)

{--表層形（surface），基本形（base），品詞（pos），品詞細分類1（pos1）のmapを作る--}
makeMecabMap [] = []
makeMecabMap (x:xs) = do
  let l = split [re|(\t|,)|] x
  let m = Map.fromList [("surface", l !! 0),("base", l !! 7), ("pos", l !! 1), ("pos1", l !! 2)]
  m : makeMecabMap xs


logify l = map (\x -> log (fromIntegral x)::Float) l

{--
[(順位, 頻度)]
--}
makeRank l = zip (logify [1..]) (logify (map (\x -> snd x) (sortBy (\x y-> compare (snd y) (snd x)) l)))


someFunc :: IO ()
someFunc = do
  text <- readFile "neko.txt"
  mecab  <- new2 ""
  result <- parse mecab text
  plotPath [(Title "Log-Log")] (makeRank $ frequency $ makeMecabMap $ init $ lines $ result)
