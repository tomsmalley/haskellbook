import Data.Char (toUpper)
import Data.List (intercalate)

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (x:xs) haystack = x `elem` haystack && isSubsequenceOf xs haystack

testIsSubsequenceOf :: Bool
testIsSubsequenceOf = True  == isSubsequenceOf "blah" "blahwoot"
                   && True  == isSubsequenceOf "blah" "wootblah"
                   && True  == isSubsequenceOf "blah" "wboloath"
                   && False == isSubsequenceOf "blah" "wootbla"


isSubsequenceOf' :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf' [] _ = True
isSubsequenceOf' _ []  = False
isSubsequenceOf' ax@(x:xs) (y:ys)
    | x == y = isSubsequenceOf' xs ys
    | otherwise = isSubsequenceOf' ax ys

testIsSubsequenceOf' :: Bool
testIsSubsequenceOf' = True  == isSubsequenceOf' "blah" "blahwoot"
                   && True  == isSubsequenceOf' "blah" "wootblah"
                   && True  == isSubsequenceOf' "blah" "wboloath"
                   && False == isSubsequenceOf' "blah" "wootbla"

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map f . words
    where f word@(x:xs) = (word, toUpper x : xs)

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs

paras :: String -> [String]
paras = map (flip (++) "." . unwords . words) . splitOn '.'

unparas :: [String] -> String
unparas = intercalate " "

splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn c s = taken : splitOn c dropped
    where dropped = drop 1 $ dropWhile (/= c) s
          taken   = takeWhile (/= c) s

capitalizeParagraph :: String -> String
capitalizeParagraph = unparas . map (capitalizeWord) . paras
