module Capitalize where

import Data.Char

capFirst :: String -> String
capFirst [] = []
capFirst (x : xs) = toUpper x : xs

capAll :: String -> String
capAll = map toUpper

capJustFirst :: String -> Maybe Char
capJustFirst [] = Nothing
capJustFirst (x:_) = Just $ toUpper x

capUnsafe :: String -> Char
capUnsafe = toUpper . head
