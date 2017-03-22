{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text    as T
import qualified Data.Text.IO as T

odds [] = []
odds [x] = []
odds (e1:e2:xs) = e2 : odds xs

main:: IO()
main = do
    let t = T.chunksOf 1 $ T.pack "パタトクカシーー"
    T.putStrLn $ T.unwords $ odds t
