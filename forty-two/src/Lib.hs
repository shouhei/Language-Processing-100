{-# LANGUAGE OverloadedStrings, QuasiQuotes, FlexibleContexts, DeriveGeneric, ScopedTypeVariables #-}
module Lib
    ( someFunc,
      dstToInt,
      dstToSrc,
      metaToDst,
      makeTuples,
      makeMorphList,
      makeChunk,
      makeChunks,
      makeChunkWithDstStr,
      Morph(..),
      Chunk(..)
    ) where

import qualified Text.CaboCha as CaboCha
import Text.Regex.PCRE.Heavy
import Data.List
import qualified Data.List.Split as LS
import Control.Applicative hiding ((<$>))
import Data.Maybe

data Morph = Morph { surface::String, base::String, pos::String, pos1::String} deriving Show
instance Eq Morph where
    (Morph w x y z) == (Morph w' x' y' z') = w == w' && x == x' && y == y' && z == z'

data Chunk = Chunk { morphs::[Morph], dst::Int, srcs::[Int]} deriving Show
instance Eq Chunk where
    (Chunk x y z) == (Chunk x' y' z') = x == x' && y == y' && z == z'

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

makeTuples [] _ _ = []
makeTuples _ [] _ = []
makeTuples _ _ [] = []
makeTuples (x:xs) (y:ys) (z:zs) = do
  (x, y, z): makeTuples xs ys zs

tToChunk x = do
  let (a,b,c) = x
  makeChunk a (metaToDst b) c

makeChunks :: [String] -> [[Chunk]]
makeChunks [] = []
makeChunks (x:xs) = do
  let metas = dstToSrc $ map (\y -> dstToInt ((words y) !! 2)) $ filter (\y -> y !! 0 == '*') $ lines $ x
  let morphLines = tail $ LS.splitWhen (\y -> y !! 0 == '*') $ lines x
  map tToChunk (makeTuples morphLines (filter (\y -> y !! 0 == '*') $ lines $ x) metas) : (makeChunks xs)

makeChunk :: [String] -> Int -> [Int] -> Chunk
makeChunk x y z = do
  let m = makeMorphList $ x
  Chunk{morphs=m, dst=y, srcs=z}

makeChunkWithDstStr :: [Chunk] -> [String]
makeChunkWithDstStr x = do
  let bases = map chunkToStr x
  let dsts = chunkDstsToStr x
  map (\y -> (fst y) ++ "\t" ++(snd y)) $ filter (\y -> (fst y) /= "" && (snd y) /= "") $ zip bases dsts

hasNoun :: Chunk -> Bool
hasNoun x = any (\y -> (pos y) == "名詞") (morphs x)

hasVerb :: Chunk -> Bool
hasVerb x = any (\y -> (pos y) == "動詞") (morphs x)

tmp :: Chunk -> [Chunk] -> Maybe String
tmp x origin
  | (dst x) /= -1 && hasNoun x && hasVerb (origin !! (dst x)) = Just $ (chunkToStr x) ++ " -> " ++ chunkToStr (origin !! (dst x)) ++ ";"
  | otherwise = Nothing

makeStringNounPertainingVerb :: [Chunk] -> [String]
makeStringNounPertainingVerb x = ["digraph g{"] ++ (map (\y -> fromJust y) $ filter (\y -> isJust y) $ map (\y -> tmp y x) x) ++ ["}"]

tmp' :: Chunk -> [Chunk] -> Maybe String
tmp' x origin
  | (dst x) /= -1 = Just $ (chunkToStr x) ++ " -> " ++ chunkToStr (origin !! (dst x)) ++ ";"
  | otherwise = Nothing

makeDiagraph :: [Chunk] -> [String]
makeDiagraph x = ["digraph g{"] ++ (map (\y -> fromJust y) $ filter (\y -> isJust y) $ map (\y -> tmp' y x) x) ++ ["}"]


someFunc :: IO ()
someFunc = do
  text <- readFile "neko.txt"
  cabocha  <- CaboCha.new ["cabocha", "-f1"]
  chunks <- makeChunks <$> mapM (\x -> CaboCha.parse cabocha x) (lines text)
  mapM_ (\x -> mapM_ putStrLn x) $ filter (\x -> length x >= 3) $ map makeDiagraph chunks
