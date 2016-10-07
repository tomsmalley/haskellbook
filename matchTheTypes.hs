import Data.List (sort)

i :: Num a => a
i = 1

f :: RealFrac a => a
f = 1.0

freud :: Ord a => a -> a
freud x = x

freud' :: Int -> Int
freud' x = x

jung :: [Int] -> Int
jung xs = head (sort xs)

young :: Ord a => [a] -> a
young xs = head (sort xs)


mySort :: [Char] -> [Char]
mySort = sort

signifier :: Ord a => [a] -> a
signifier xs = head (mySort xs)
