module Jammin where

import Data.List

data Fruit = Peach
           | Plum
           | Apple
           | Blackberry
           deriving (Eq, Show, Ord)

data JamJars = Jam { fruit :: Fruit
                   , jars :: Int
                   } deriving (Eq, Show, Ord)

row1 = Jam Peach 20
row2 = Jam Plum 13
row3 = Jam Apple 1
row4 = Jam Blackberry 38
row5 = Jam Peach 10
row6 = Jam Blackberry 22
allJam = [row1, row2, row3, row4, row5, row6]

jarCount :: Int
jarCount = sum $ map jars allJam

mostRow :: JamJars
mostRow = foldr (\x acc -> if jars x > jars acc then x else acc) (Jam Peach 0) allJam

sortJam = sortBy (\x y -> compare (fruit x) (fruit y)) allJam

groupJam = groupBy (\x y -> fruit x == fruit y) sortJam
