module Main where

import qualified MyLib (someFunc)
import qualified Baby 

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc
  Baby.someFunc 
