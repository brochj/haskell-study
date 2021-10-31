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

split :: Char -> String -> [String]
split _ "" = []
split delimiter str =
    let (start, rest) = break (== delimiter) str
        (_, remain) = span (== delimiter) rest -- this line removes the delimiter from rest
     in start : split delimiter remain

splitOn :: String -> String -> [String]
splitOn _ "" = []
splitOn delimiters str =
    let (start, rest) = break (`elem` delimiters) str
        (_, remain) = span (`elem` delimiters) rest -- this line removes the delimiter from rest
     in start : splitOn delimiters remain

sentences :: String -> [String]
sentences = splitOn ".?!\n;" 