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


{--
出現回数が同じの単語が何個あるかを作成する
[(出現回数 種類)]
--}
makeFrequencyList :: [(String, Int)] -> [(Int, Int)]
makeFrequencyList [] = []
makeFrequencyList (x:xs) = do
  let num = length $ filter (\y -> (snd x) == (snd y)) xs
  ((snd x), num + 1): (makeFrequencyList (filter (\y -> (snd x) /= (snd y)) xs))

{--
グラフ用にラベルを作る
('a0出現回数' 0, 'a1出現回数' 1,...) :: String
--}
makeLabel l = do
  let w = map (\x -> fst x) l
  let t = zip w [0..]
  let text = init $ unwords $ map (\x -> "'" ++ (show (fst x)) ++ "'" ++ (show (snd x)) ++ ",") t
  "(" ++ text ++ ")"

someFunc :: IO ()
someFunc = do
  text <- readFile "neko.txt"
  mecab  <- new2 ""
  result <- parse mecab text
  let l = makeMecabMap $ init $ lines result
  {-- 出現回数が1,2,3... でならべた [(出現回数 種類)]--}
  let f =  sortBy (\x y-> compare (fst x) (fst y)) $ makeFrequencyList $  (frequency l)
  plotPathStyle [(Title "Frequency"), (XTicks (Just [makeLabel f]))] (defaultStyle{plotType = Boxes })  $ (zip [0..] (map (\x -> snd x) f))
