{-# LANGUAGE OverloadedStrings, QuasiQuotes, FlexibleContexts, DeriveGeneric, ScopedTypeVariables #-}
module Lib
    ( someFunc
    ) where

import qualified Text.CaboCha as CaboCha
import Text.Regex.PCRE.Heavy
import Data.List
import qualified Data.List.Split as LS

data Morph = Morph { surface::String, base::String, pos::String, pos1::String} deriving Show
data Chunk = Chunk { morphs::[Morph], dst::Int, srcs::[Int]} deriving Show

makeMorphList :: [String] -> [Morph]
makeMorphList [] = []
makeMorphList (x:xs) = do
  let l = split [re|(\t|,)|] x
  if (length l) < 7 then
        makeMorphList xs
  else
      Morph {surface = (l !! 0), base = (l !! 7), pos = (l !! 1), pos1 = (l !! 2) } : makeMorphList xs

dstToInt :: String -> Int
dstToInt x
  | (x !! 0) == '-' = -1
  | otherwise       = read $ init x

dstToSrc :: [Int] -> [[Int]]
dstToSrc x = map (\y -> elemIndices y x) [0..]

{--makeChunks :: [String] -> [[Chunk]]
makeChunks [] = []
makeChunks (x:xs) = do
  let metas = map (\y -> (y !! 4)) $ filter (\y -> (y !! 0) == '*' ) $ lines x
  let m = map (\y -> read y) metas
  let morphLines = LS.splitWhen (\y -> y !! 0 == '*') $ lines x
  (map (\y -> makeChunk (fst y) (snd y)) $ zip morphLines (dstToSrc metas) )--}

makeChunk :: [String] -> [Int] -> Chunk
makeChunk x y = do
  let c = words $ x !! 0
  let m = makeMorphList $ tail x
  Chunk{morphs=m, dst=(dstToInt (c !! 2)), srcs=y}

someFunc :: IO ()
someFunc = do
  text <- readFile "neko.txt"
  cabocha  <- CaboCha.new ["cabocha", "-f1"]
  chunkStr <- mapM (\x -> CaboCha.parse cabocha x) (lines text)
  mapM_ print $ dstToSrc $ map (\x -> dstToInt ((words x) !! 2)) $ filter (\x -> x !! 0 == '*') $ lines $ chunkStr !! 7
  {--let chunks = makeChunks chunkStr
  print $ chunks !! 7
  putStrLn $ surface (morphs ((chunks !! 7)!!0) !! 0)--}
