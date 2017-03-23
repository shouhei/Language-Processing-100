module Lib
    ( someFunc
    ) where
import Data.List.Split
import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as T

template name =
  "xa" ++ [name]

wirteEachLinesToFile name n [] = do return ()
writeEachLinesToFile name n l
  | n > length l = do
      T.writeFile (template name) (T.concat $ map (\x -> T.append x (T.pack "\n")) (map T.pack l))

  | otherwise = do
      T.writeFile (template name) $ T.concat $ map (\x -> T.append x (T.pack "\n")) (map T.pack (take n l))
      let next_name = toEnum (1 + fromEnum name) :: Char
      writeEachLinesToFile next_name n (drop n l)

someFunc :: IO ()
someFunc = do
  text <- readFile "hightemp.txt"
  let l = init $ splitOn "\n" text
  args <- getArgs
  writeEachLinesToFile 'a' (read (args !! 0)) l
