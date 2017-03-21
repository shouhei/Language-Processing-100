module Lib
    ( someFunc,
      orderByRandom,
      typoglycemia
    ) where

import System.Random

orderByRandom [] = return []
orderByRandom x
  | length x == 1 = return x
  | otherwise     = do
    r <- getStdRandom $ randomR (0, (length x) - 1) :: IO Int
    others <- orderByRandom ((take (r) x) ++ (drop (r+1) x))
    return $ (x !! r) : others

typoglycemia f x
  | length x < 4 = return x
  | otherwise    = do
    let h = head x
    let l = last x
    middle <- f $ init $ tail x
    return $ h : middle ++ [l]

showIO [] = putStrLn ""
showIO x = do
  putStr =<< x !! 0
  putStr " "
  showIO (tail x)

someFunc :: IO ()
someFunc = do
  let t = "I couldn't believe that I could actually understand what I was reading : the phenomenal power of the human mind ."
  let result = map (typoglycemia orderByRandom) (words t)
  showIO result
