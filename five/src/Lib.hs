module Lib
    ( someFunc
    ) where

import qualified Data.Map as Map

validate s = if last s == ',' || last s == '.' then init s else s

someFunc :: IO ()
someFunc = do
  let t = Prelude.map validate $ words "Hi He Lied Because Boron Could Not Oxidize Fluorine. New Nations Might Also Sign Peace Security Clause. Arthur King Can."
  let def = [1, 2, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 1, 1, 2, 1]
  let shorts = Prelude.map (\(a, b) -> if length b > 1 then (take a b) else b) $ zip def t
  let n = [1 .. length shorts]
  let dict = Map.fromList $ zip shorts n
  print dict
  print $ Map.lookup "H" dict
