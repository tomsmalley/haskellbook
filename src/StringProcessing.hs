module StringProcessing where

import Data.Maybe (fromMaybe)

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe x = Just x

replaceThe :: String -> String
replaceThe = unwords . map (fromMaybe "a" . notThe) . words

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = go 0 . map notThe . words
    where go :: Integer -> [Maybe String] -> Integer
          go acc [] = acc
          go acc (_:[]) = acc
          go acc ((Nothing):(Just x):xs) = if head x `elem` "aeiou"
                                              then go (acc + 1) xs
                                              else go acc xs
          go acc (x:xs) = go acc xs

countVowels :: String -> Integer
countVowels = toInteger . length . filter (flip elem "aeiou")
