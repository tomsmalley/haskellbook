module WordNumber where

import Data.List (intercalate)

digitToWord :: Int -> String
digitToWord x
    | x < 0 = "minus-" ++ (digitToWord $ negate x)
    | x == 0 = "zero"
    | x == 1 = "one"
    | x == 2 = "two"
    | x == 3 = "three"
    | x == 4 = "four"
    | x == 5 = "five"
    | x == 6 = "six"
    | x == 7 = "seven"
    | x == 8 = "eight"
    | x == 9 = "nine"
    | otherwise = "err"

digits :: Int -> [Int]
digits n
    | abs n < 10 = [n]
    | otherwise = digits (quot n 10) ++ [abs (rem n 10)]

digits' :: Int -> [Int]
digits' x = map (\x -> read [x]) $ show x

wordNumber :: Int -> String
wordNumber = intercalate "-" . map digitToWord . digits
