module Lib
    ( someFunc
    ) where

encrypt x =
  if fromEnum x >= 97 && 122 >= fromEnum x then
    toEnum (219 - fromEnum x)
  else
    x

decrypt x = encrypt x

someFunc :: IO ()
someFunc = do
  let initial = "ABCabc"
  let after_encrypt = map encrypt initial
  let after_decrypt = map decrypt after_encrypt
  putStrLn after_encrypt
  putStrLn after_decrypt
