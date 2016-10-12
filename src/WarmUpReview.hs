module WarmUpReview where

import Control.Applicative (liftA3)

stops = "pbtdkg"
vowels = "aeiou"

tripleTuple :: [(Char, Char, Char)]
tripleTuple = [(x, y, z) | x <- stops, y <- vowels, z <- stops]

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)

ptripleTuple :: [(Char, Char, Char)]
ptripleTuple = [(x, y, z) | x <- "p", y <- vowels, z <- stops]

nouns = [ "train"
        , "boat"
        , "sign"
        , "bag"
        , "drink"
        , "table"
        ]
verbs = [ "run"
        , "take"
        , "drop"
        , "say"
        , "make"
        ]

sentences :: [(String, String, String)]
sentences = [(x, y, z) | x <- nouns, y <- verbs, z <- nouns]

seekritFunc x = top / bot
    where top = fromIntegral . sum . map length . words $ x
          bot = fromIntegral . length . words $ x
