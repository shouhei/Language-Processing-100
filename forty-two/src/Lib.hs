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
import Debug.Trace

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
hasNoun x = any isNoun $ morphs x

isNoun :: Morph -> Bool
isNoun x = pos x == "名詞"

hasSahenSetsuzoku :: Chunk -> Bool
hasSahenSetsuzoku x = any isSahenSetsuzoku $ morphs x

isSahenSetsuzoku :: Morph -> Bool
isSahenSetsuzoku x = pos1 x == "サ変接続"

hasVerb :: Chunk -> Bool
hasVerb x = any isVerb $ morphs x

isVerb :: Morph -> Bool
isVerb x = pos x == "動詞"

hasPostpositionalParticle :: Chunk -> Bool
hasPostpositionalParticle x = any isPostpositionalParticle $ morphs x

isPostpositionalParticle :: Morph -> Bool
isPostpositionalParticle x = pos x == "助詞"

isWo :: Morph -> Bool
isWo x = surface x == "を"

getSrcChunks :: [Chunk] -> [Int] -> [Chunk]
getSrcChunks _ [] = []
getSrcChunks x (y:ys) = x !! y : getSrcChunks x ys

getVerbFromMorphs :: Chunk -> Morph
getVerbFromMorphs x = fromJust $ find (\y -> pos y == "動詞") $ morphs x

getPostpositionalParticle :: Chunk -> String
getPostpositionalParticle x = do
  let morph = find (\y -> pos y == "助詞") $ morphs x
  if isJust morph  then
    base $ fromJust morph
  else
    ""

makeVerbPPPair :: Chunk -> [Chunk] -> String
makeVerbPPPair x y = do
  let verb =filter isVerb $ morphs x
  let fvs = filter hasFunctionalVerbSyntax y
  if null verb || null fvs then
    ""
  else
    (chunkToStr (last fvs)) ++ (surface (last verb)) ++ "\t" ++(unwords (map (\z -> surface z ++ " ") (map (\z -> last (filter isPostpositionalParticle (morphs z))) $ filter (\z -> z /= (last fvs)) y))) ++ "\t" ++ (unwords (map (\z -> chunkToStr z ++ " ") (filter (\z -> z /= (last fvs)) y)))

hasFunctionalVerbSyntax :: Chunk -> Bool
hasFunctionalVerbSyntax x = do
  let wo = find isWo $ morphs x
  if wo == Nothing then
    False
  else
    hasSahenSetsuzoku x && hasPostpositionalParticle x

extractChunkHasVerb :: [Chunk] -> [String]
extractChunkHasVerb x = do
  let hasVerbChunk = filter hasVerb x
  let chunks = map (\y -> getSrcChunks x y) $ map srcs hasVerbChunk
  map (\y -> makeVerbPPPair (fst y) (snd y)) $ zip hasVerbChunk chunks

someFunc :: IO ()
someFunc = do
  text <- readFile "sample.txt"
  cabocha  <- CaboCha.new ["cabocha", "-f1"]
  chunks <- makeChunks <$> mapM (\x -> CaboCha.parse cabocha x) (lines text)
  mapM_ (\x -> mapM_ putStrLn (filter (\y -> y /= "") x)) $ map extractChunkHasVerb chunks
