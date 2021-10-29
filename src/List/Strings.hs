module List.Strings where

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase str = [c | c <- str, c `elem` ['A'..'Z']]
-- removeNonUppercase = filter (`elem` ['A'..'Z'])