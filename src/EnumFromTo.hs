module EnumFromTo where

eftBool :: Bool -> Bool -> [Bool]
eftBool False False = [False]
eftBool False True = [False, True]
eftBool True False = []
eftBool True True = [True]


eftChar :: Char -> Char -> [Char]
eftChar a b
    | a > b = []
    | otherwise = a : eftChar (succ a) b

enumFromTo' :: (Ord a, Enum a) => a -> a -> [a]
enumFromTo' a b
    | a > b = []
    | a == b = [b]
    | otherwise = a : enumFromTo' (succ a) b
