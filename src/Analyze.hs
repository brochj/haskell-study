module Analyze where
import Data.List (nub)

removePunctuation :: String -> String
removePunctuation = filter (`notElem` chars)
    where chars = "!\"#$%&()*+,./:;<=>?@[\\]^_`{|}~0123456789"

allWords :: String -> [String]
allWords = Prelude.words

uniqueWords :: [String] -> [String]
uniqueWords = nub

words :: String -> [String]
words = uniqueWords . allWords . removePunctuation

characters :: String -> Int
characters = length

nonSpaceCharacters :: String -> Int
nonSpaceCharacters = length . filter (/=' ')

lines :: String -> [String]
lines = Prelude.lines

numberOfLines :: String -> Int
numberOfLines = length . Prelude.lines


replace :: Char -> Char -> String -> String
-- replace old new str = [if c == old then new else c | c <- str]
replace old new  = map (\c -> if c == old then new else c)

-- sentences :: String -> [String]
-- sentences = filter . replace '?' '.'
