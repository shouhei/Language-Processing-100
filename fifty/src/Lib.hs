{-# LANGUAGE OverloadedStrings, QuasiQuotes, FlexibleContexts, DeriveGeneric, ScopedTypeVariables #-}
module Lib
    ( someFunc
    ) where

import Text.Regex.PCRE.Heavy
import qualified Data.List as L
import Debug.Trace

takeThree :: String -> Int -> String
takeThree txt origin
  | length txt < 3 = txt
  | otherwise = (txt !! (origin - 2)) : (txt !! (origin-1)) : (txt !! origin) : []

splitText :: String -> [String]
splitText [] = []
splitText txt = do
  let a = L.break (\x -> (takeThree txt x) =~ [re|^(\.|;|:|\?|\!)\s[A-Z]$|] ) [2..((length txt) -1)]
  (init (take (length (fst a)) txt)) : splitText (drop (length (fst a) + 2) txt)

addLn :: [String] -> [String]
addLn x = x ++ ["\NUL"]

someFunc :: IO ()
someFunc = do
  txt <- readFile "nlp.txt"
  mapM_ (\t -> mapM_ putStrLn (addLn (words t))) $ splitText txt
