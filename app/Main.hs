module Main where

import qualified MyLib (someFunc)
import qualified Baby 
import qualified Analyze (words)

main :: IO ()
main = do
  -- putStrLn "Hello, Haskell!! I'm from New York. New York is a beautiful city"
  print $ Analyze.words "Hello, Haskell!! I'm from Ney York. \nNew York is a beautiful city"