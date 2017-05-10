{-# LANGUAGE OverloadedStrings, QuasiQuotes, FlexibleContexts, DeriveGeneric, ScopedTypeVariables #-}
module Lib
    ( someFunc
    ) where

import qualified Text.CaboCha as CaboCha
import Text.Regex.PCRE.Heavy
import qualified Data.List.Split as LS

data Morph = Morph { surface::String, base::String, pos::String, pos1::String} deriving Show
data Chunk = Chunk { morphs::[Morph], dst::Int, srcs::Int} deriving Show

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


figureToSentence :: [String] -> [[String]]
figureToSentence [] = []
figureToSentence (x:xs) = do
  let base = break (\y -> (y !! 0) == '*') xs
  [x:(fst base)] ++ figureToSentence (snd base)

makeChunk :: [String] -> Chunk
makeChunk x = do
  let c = words $ x !! 0
  let m = makeMorphList $ tail x
  Chunk{morphs=m, dst=(dstToInt (c !! 2)), srcs=0}

someFunc :: IO ()
someFunc = do
  text <- readFile "neko.txt"
  cabocha  <- CaboCha.new ["cabocha", "-f1"]
  chunks <- mapM (\x -> CaboCha.parse cabocha x) (lines text)
  putStrLn $ surface ((morphs (makeChunk $ lines (chunks !! 7))) !! 0)
