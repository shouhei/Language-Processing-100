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
import qualified Data.ByteString as B
import Data.ByteString.Lazy.UTF8 as U8
import qualified Data.Map as Map

data Article = Article {
  title :: String,
  text :: String } deriving (Eq, Show, Generic)
instance FromJSON Article

makeTuple [] = ("", "")
makeTuple (x:[]) = (x, "")
makeTuple (x:xs) = (x, xs !! 0)

makeMatchList [] = return ("", "")
makeMatchList (x:xs) = do
  if x =~ [re|^.*\s=\s.*$|] then
    makeTuple (splitOn " = " $ tail x) : (makeMatchList xs)
  else
   makeMatchList xs

someFunc :: IO ()
someFunc = do
  body <- fromString <$> readFile "jawiki-country.json"
  let result = filter (\x -> (title x) == "イギリス") (map (\x -> fromJust $ A.decode x) $ U8.lines body)
  let l = makeMatchList $ splitOn "\n" $ text $ result !! 0
  let dict =  Map.fromList $ init l
  putStrLn $ fromJust $ Map.lookup "通貨" dict
