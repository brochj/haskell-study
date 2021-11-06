module Analyze where
import Data.List (nub)
import Debug.Trace (trace)

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

nonSpaceCharacters :: String -> String
nonSpaceCharacters = filter (/=' ')

numberOfnonSpaceCharacters :: String -> Int
numberOfnonSpaceCharacters = length . filter (/=' ')

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

calculateARI :: String -> Int
calculateARI text = ceiling (term1 + term2)
   where
       chars = fromIntegral $ numberOfnonSpaceCharacters text
       numberOfWords = fromIntegral . length $ Analyze.words text
       numberOfsentences = fromIntegral . length $ sentences text
       term1 = 4.71 * ( chars / numberOfWords )
       term2 = 0.5 * (numberOfWords / numberOfsentences) - 21.43

meaningARI :: Int -> IO ()
meaningARI indexARI = putStrLn $ findKey parameterizedARI meaning_string
    where
        parameterizedARI = if indexARI <= 14 then indexARI else 14
        meaning_string =
            [(1,"5-6 : Kindergarten")
            ,(2,"6-7 : First/Second Grade")
            ,(3,"7-9 : Third Grade")
            ,(4,"9-10 : Fourth Grade")
            ,(5,"10-11 : Fifth Grade")
            ,(6,"11-12 : Sixth Grade")
            ,(7,"12-13 : Seventh Grade")
            ,(8,"13-14 : Eighth Grade")
            ,(9,"14-15 : Ninth Grade")
            ,(10,"15-16 : Tenth Grade")
            ,(11,"16-17 : Eleventh Grade")
            ,(12,"17-18 : Twelfth grade")
            ,(13,"18-24 : College student")
            ,(14,"24+ : Professor")
            ]

findKey :: (Eq k) => k -> [(k,v)] -> v
findKey key = snd . head . filter (\(k,v) -> key == k)

numberOfRectangles ::  Int -> Int -> Int
numberOfRectangles lin col = sum [1..lin] * sum [1..col]


validBraces :: String -> Bool
validBraces s = "" == foldr collapse [] s

collapse :: Char -> [Char] -> [Char]
collapse '(' (')':xs) = xs
collapse '{' ('}':xs) = xs
collapse '[' (']':xs) = xs
collapse x xs = x:xs

