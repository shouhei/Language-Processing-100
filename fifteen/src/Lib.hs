module Lib
    ( someFunc
    ) where

import Data.List.Split
import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as T

someFunc :: IO ()
someFunc = do
  text <- readFile "hightemp.txt"
  let l = init $ splitOn "\n" text
  args <- getArgs
  let result = drop ((length l ) - read (args !! 0)) l
  T.putStr $ T.concat $ map (\x -> T.append x (T.pack "\n")) (map T.pack result)

