module Phone where

import Data.List (intercalate, elemIndex, group, maximumBy, sort)
import Data.Char (toLower, isUpper, isAlphaNum)
import Data.Maybe (fromJust)

data Phone = Phone [Button]

instance (Show Phone) where
    show (Phone buttons) = intercalate "\n" $ map show buttons

data Button = Button Digit [Char]
    deriving (Show)

type Digit = Char
type Presses = Int

reverseTaps :: Phone -> Char -> [(Digit, Presses)]
reverseTaps (Phone []) _ = []
reverseTaps p@(Phone ((Button d cs):bs)) c
    | c == '*'  = [('*', 2)]
    | isUpper c = ('*', 1) : reverseTaps p (toLower c)
    | otherwise = if c `elem` cs ++ [d]
                     then [(d, fromJust (elemIndex c (cs ++ [d])) + 1)]
                     else reverseTaps (Phone bs) c

cellPhonesDead :: Phone -> String -> [(Digit, Presses)]
cellPhonesDead p = concatMap (reverseTaps p)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd

frequent :: (Ord a) => [a] -> a
frequent = fst . maximumBy f . map g . group . sort
    where f x y = compare (snd x) (snd y)
          g x   = (head x, length x)

mostPopularLetter :: String -> Char
mostPopularLetter = frequent

coolestLtr :: [String] -> Char
coolestLtr = frequent . filter (isAlphaNum) . concat

coolestWord :: [String] -> String
coolestWord = frequent . words . concat

phone = Phone [ Button '1' ""
              , Button '2' "abc"
              , Button '3' "def"
              , Button '4' "ghi"
              , Button '5' "jkl"
              , Button '6' "mno"
              , Button '7' "pqrs"
              , Button '8' "tuv"
              , Button '9' "wxyz"
              , Button '*' ""
              , Button '0' " +"
              , Button '#' ".,"
              ]

convo :: [String]
convo =
       ["Wanna play 20 questions",
        "Ya",
        "U 1st haha",
        "Lol ok. Have u ever tasted alcohol lol",
        "Lol ya",
        "Wow ur cool haha. Ur turn",
        "Ok. Do u think I am pretty Lol",
        "Lol ya",
        "Haha thanks just making sure rofl ur turn"]
