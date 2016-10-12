module CurryEx where

excl :: String -> String
excl x = x ++ "!"

fifth :: String -> String
fifth x = take 1 $ drop 4 x

drop2words :: String -> String
drop2words x = unwords . drop 2 $ words x

thirdChar :: String -> Char
thirdChar x = x !! 2

letterIndex :: Int -> Char
letterIndex x = "Curry is awesome!" !! x

revWords :: String -> String
revWords = unwords . reverse . words

revLong :: String -> String
revLong x = drop 9 x ++ take 4 (drop 5 x) ++ take 5 x
