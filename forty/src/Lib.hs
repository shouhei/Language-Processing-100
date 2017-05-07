{-# LANGUAGE OverloadedStrings, QuasiQuotes, FlexibleContexts, DeriveGeneric, ScopedTypeVariables #-}
module Lib
    ( someFunc
    ) where

import Text.CaboCha
import Text.Regex.PCRE.Heavy
import qualified Data.List.Split as LS

data Morph = Morph { surface::String, base::String, pos::String, pos1::String} deriving Show

makeMorphStr x = "surface: " ++ (surface x) ++ ", base: " ++ (base x) ++ ", pos: " ++ (pos x) ++ ", pos1: " ++ (pos1 x)

makeMorphList [] = []
makeMorphList (x:xs) = do
  if (x !! 0) /= '*' then
      let l = split [re|(\t|,)|] x in
      Morph {surface = l !! 0, base = l !! 7, pos = l !! 1, pos1 = l !! 2 } : makeMorphList xs
  else
    makeMorphList xs

someFunc :: IO ()
someFunc = do
  text <- readFile "neko.txt"
  mecab  <- new ["cabocha", "-f1"]
  result <- parse mecab text
  let l = LS.splitWhen (\x -> x !! 0 == '*') (lines result)
  mapM_ (\x -> putStrLn (makeMorphStr x)) ((map makeMorphList l) !! 2)
