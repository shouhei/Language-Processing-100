{-# LANGUAGE OverloadedStrings, QuasiQuotes, FlexibleContexts, DeriveGeneric, ScopedTypeVariables #-}
module Lib
    ( someFunc
    ) where

import qualified Text.CaboCha as CaboCha
import Text.Regex.PCRE.Heavy
import Data.List
import qualified Data.List.Split as LS
import Debug.Trace

data Morph = Morph { surface::String, base::String, pos::String, pos1::String} deriving Show
data Chunk = Chunk { morphs::[Morph], dst::Int, srcs::[Int]} deriving Show

chunkToStr :: Chunk -> String
chunkToStr x = foldl (\y z -> y ++ (surface z)) "" (morphs x)

chunkDstsToStr :: [Chunk] -> [String]
chunkDstsToStr x = do
  let srcsData = map (\y -> dst y) x
  map (\y ->
         if y == -1 then
             ""
         else
            chunkToStr (x !! y)
      ) srcsData

makeMorphList :: [String] -> [Morph]
makeMorphList [] = []
makeMorphList (x:xs) = do
  let l = split [re|(\t|,)|] x
  if ((length l) < 7) || (l !! 1) == "記号" then
        makeMorphList xs
  else
      Morph {surface = (l !! 0), base = (l !! 7), pos = (l !! 1), pos1 = (l !! 2) } : makeMorphList xs

dstToInt :: String -> Int
dstToInt x
  | (x !! 0) == '-' = -1
  | otherwise       = read $ init x

dstToSrc :: [Int] -> [[Int]]
dstToSrc x = map (\y -> elemIndices y x) [0..(length x)]

metaToDst :: String -> Int
metaToDst x = dstToInt  $ (words x) !! 2

makeTuple [] _ _ = []
makeTuple _ [] _ = []
makeTuple _ _ [] = []
makeTuple (x:xs) (y:ys) (z:zs) = do
  (x, y, z): makeTuple xs ys zs

tToChunk x = do
  let (a,b,c) = x
  makeChunk a (metaToDst b) c

makeChunks :: [String] -> [[Chunk]]
makeChunks [] = []
makeChunks (x:xs) = do
  let metas = dstToSrc $ map (\y -> dstToInt ((words y) !! 2)) $ filter (\y -> y !! 0 == '*') $ lines $ x
  let morphLines = tail $ LS.splitWhen (\y -> y !! 0 == '*') $ lines x
  map tToChunk (makeTuple morphLines (filter (\y -> y !! 0 == '*') $ lines $ x) metas) : (makeChunks xs)

makeChunk :: [String] -> Int -> [Int] -> Chunk
makeChunk x y z = do
  let m = makeMorphList $ x
  Chunk{morphs=m, dst=y, srcs=z}

printChunkWithDst :: [Chunk] -> IO()
printChunkWithDst x = do
  let bases = map chunkToStr x
  let dsts = chunkDstsToStr x
  mapM_ putStrLn $ map (\y -> (fst y) ++ "\t" ++(snd y)) $ filter (\y -> (fst y) /= "" && (snd y) /= "") $ zip bases dsts

someFunc :: IO ()
someFunc = do
  text <- readFile "neko.txt"
  cabocha  <- CaboCha.new ["cabocha", "-f1"]
  chunkStr <- mapM (\x -> CaboCha.parse cabocha x) (lines text)
  let chunks = makeChunks chunkStr
  mapM_ printChunkWithDst chunks
