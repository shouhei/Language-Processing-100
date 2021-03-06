{-# LANGUAGE OverloadedStrings, QuasiQuotes, FlexibleContexts, DeriveGeneric #-}
module Lib
    ( someFunc
    ) where

import Data.List.Split
import Text.Regex.PCRE.Heavy
import Data.Aeson as A
import GHC.Generics
import Data.Maybe
import Data.String.Utils
import Data.ByteString.Lazy.UTF8 as U8

data Article = Article {
  title :: String,
  text :: String } deriving (Eq, Show, Generic)
instance FromJSON Article

printEach [] = return ()
printEach (x:xs) = do
  let tmp = (scan [re|ファイル:.*\..[a-zA-Z]*|] x)
  if tmp /= [] then
    putStrLn $ gsub [re|ファイル:|] (" "::String) $ fst $ head $ tmp
  else
    return ()
  printEach xs

someFunc :: IO ()
someFunc = do
  body <- fromString <$> readFile "jawiki-country.json"
  let result = filter (\x -> (title x) == "イギリス") (map (\x -> fromJust $ A.decode x) $ U8.lines body)
  printEach $ splitOn "\n" $ text $ result !! 0
