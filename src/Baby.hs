module Baby where
import Data.List (nub)

someFunc :: IO ()
someFunc = putStrLn "Hello from Baby"

doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100 then x else x * 2

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <- st, c `elem` ['A' .. 'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

first :: (Show a, Integral b, Integral c) => (a, b, c) -> a
first (x, _, _) = x

length' :: (Floating a, Integral b) => [a] -> b
length' [] = 0
length' (_ : xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x : xs) = x + sum' xs

bmi :: (RealFloat a) => a -> a -> a
bmi weight height = weight / height ^ 2

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi weight height <= 18.5 = "You're underweight, you emo, you!"
  | bmi weight height <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi weight height <= 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"

max' :: (Ord a) => a -> a -> a
max' a b
  | a > b = a
  | otherwise = b

maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)   
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = maximum' xs  

replicate' :: (Num i, Ord i) => i -> a -> [a]  
replicate' n x  
    | n <= 0    = []  
    | otherwise = x:replicate' (n-1) x  

compareWithHundred :: (Num a, Ord a) => a -> Ordering  
compareWithHundred = compare 100  

multThree :: (Num a) => a -> a -> a -> a  
multThree x y z = x * y * z

zipWith' :: (t1 -> t2 -> a) -> [t1] -> [t2] -> [a]
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys 

maskify :: String -> String
maskify x 
  | length x <= 4 = x
  | otherwise = "#" ++ maskify (tail x)

