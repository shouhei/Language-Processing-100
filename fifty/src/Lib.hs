{-# LANGUAGE OverloadedStrings, QuasiQuotes, FlexibleContexts, DeriveGeneric, ScopedTypeVariables #-}
module Lib
    ( someFunc
    ) where

import Text.Regex.PCRE.Heavy
import qualified Data.List as L

takeThree :: String -> Int -> String
takeThree txt origin
  | length txt < 3 = txt
  | otherwise = (txt !! (origin - 2)) : (txt !! (origin-1)) : (txt !! origin) : []

splitText :: String -> [String]
splitText [] = []
splitText txt = do
  let a = L.break (\x -> (takeThree txt x) =~ [re|^(\.|;|:|\?|!)\S[A-Z]$|] ) [2..((length txt) -1)]
  (take (length (fst a)) txt): splitText (drop (length (fst a)) txt)

someFunc :: IO ()
someFunc = do
  txt <- readFile "nlp.txt"
  mapM_ putStrLn $ splitText txt
