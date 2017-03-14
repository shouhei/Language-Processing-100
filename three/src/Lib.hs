
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import qualified Data.Text    as T
import qualified Data.Text.IO as T

someFunc :: IO ()
someFunc = do
    let p = T.chunksOf 1 $ T.pack "パトカー"
    let t = T.chunksOf 1 $ T.pack "タクシー"
    T.putStrLn $ T.concat $ zipWith (T.append) p t
