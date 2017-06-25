module Lib
    ( someFunc
    ) where

import Text.XML.Light

someFunc :: IO ()
someFunc = do
  xmlText <- readFile "sample.xml"
  case parseXMLDoc xmlText of
    Nothing -> error "parse error"
    Just root -> putStrLn "parsed"
