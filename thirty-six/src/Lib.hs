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

frequency (_:[]) = [("", 0)]
frequency (x:xs) = do
  let tmp = filter (\y -> (base x) == (base y)) xs
  (base x, length(tmp)+1): frequency (filter (\y -> (base x) /= (base y)) xs)

makeMecabMap [] = [Map.fromList [("surface", ""), ("base", ""), ("pos", ""), ("pos1", "")]]
makeMecabMap (x:xs) = do
  let l = split [re|(\t|,)|] x
  let m = Map.fromList [("surface", l !! 0),("base", l !! 7), ("pos", l !! 1), ("pos1", l !! 2)]
  m : makeMecabMap xs

topTen l =
  take 10 $ sortBy (\x y-> compare (snd y) (snd x)) $ frequency l

makeData l =
  zip [0..] (map (\x -> snd x) $ topTen l)

makeLabel l = do
  let w = map (\x -> fst x) (topTen l)
  let t = zip w [0..]
  let text = init $ unwords $ map (\x -> "'" ++ (fst x) ++ "'" ++ (show (snd x)) ++ ",") t
  "(" ++ text ++ ")"

someFunc :: IO ()
someFunc = do
  text <- readFile "neko.txt"
  mecab  <- new2 ""
  result <- parse mecab text
  let l = makeMecabMap $ init $ lines result
  plotPathStyle [(Title "Frequency"), (XTicks (Just [(makeLabel l)]))] (defaultStyle{plotType = Boxes }) $ makeData l
