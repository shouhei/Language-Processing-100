{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Lib
    ( someFunc
    ) where

import Data.Aeson as A
import GHC.Generics
import Data.Maybe
import Data.ByteString.Lazy.UTF8 as U8

data Article = Article {
  title :: String,
  text :: String } deriving (Eq, Show, Generic)
instance FromJSON Article

someFunc :: IO ()
someFunc = do
  body <- fromString <$> readFile "jawiki-country.json"
  let l = U8.lines body
  let result = filter (\x -> (title x) == "イギリス") (map (\x -> fromJust $ A.decode x) l)
  putStrLn $ text $ result !! 0










