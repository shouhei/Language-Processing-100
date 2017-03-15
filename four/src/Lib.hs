module Lib
    ( someFunc
    ) where

import Data.List

validate s = if last s == ',' || last s == '.' then init s else s

someFunc :: IO ()
someFunc = do
  let t = "Now I need a drink, alcoholic of course, after the heavy lectures involving quantum mechanics."
  let s = sortBy (\x y -> compare (length x) (length y)) (map validate $ words t)
  putStrLn $ (intercalate "\n" s)
