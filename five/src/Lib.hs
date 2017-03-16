module Lib
    ( someFunc
    ) where

import qualified Data.Map as Map

validate s = if last s == ',' || last s == '.' then init s else s

someFunc :: IO ()
someFunc = do
  let t = Prelude.map validate $ words "Hi He Lied Because Boron Could Not Oxidize Fluorine. New Nations Might Also Sign Peace Security Clause. Arthur King Can."
  let def = [2, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 1, 1, 2, 1]
  let dict = Map.fromList $ Prelude.map (\(a, b) -> ((take a b),b)) $ zip def t
  print dict
  print $ Map.lookup "H" dict
