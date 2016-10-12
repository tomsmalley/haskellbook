module FilterDog where

myFilter :: [String] -> String -> [String]
myFilter dict = filter (not . flip elem dict) . words
