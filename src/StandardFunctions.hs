module StandardFunctions where

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs)
    | x == True = True
    | otherwise = myOr xs

myOr' :: [Bool] -> Bool
myOr' = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs)
    | f x == True = True
    | otherwise = myAny f xs

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f = foldr ((||) . f) False

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e (x:xs)
    | e == x = True
    | otherwise = myElem e xs

myElemF :: Eq a => a -> [a] -> Bool
myElemF e = foldr (\x acc -> x == e || acc) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' = any . (==)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myReverse' :: [a] -> [a]
myReverse' = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x acc -> if f x then x : acc else acc) []

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishF :: [[a]] -> [a]
squishF = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . map f

squishMapF :: (a -> [b]) -> [a] -> [b]
squishMapF f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "empty list"
myMaximumBy _ (x:[]) = x
myMaximumBy f (x1:x2:xs) =
    case f x1 x2 of
        GT -> myMaximumBy f (x1:xs)
        _ -> myMaximumBy f (x2:xs)

myMaximumByF :: (a -> a -> Ordering) -> [a] -> a
myMaximumByF _ [] = error "empty list"
myMaximumByF f (x:xs) = foldr (\x acc -> if f x acc == GT then x else acc) x xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "empty list"
myMinimumBy _ (x:[]) = x
myMinimumBy f (x1:x2:xs) =
    case f x1 x2 of
        LT -> myMinimumBy f (x1:xs)
        _ -> myMinimumBy f (x2:xs)

myMinimumByF :: (a -> a -> Ordering) -> [a] -> a
myMinimumByF _ [] = error "empty list"
myMinimumByF f (x:xs) = foldr (\x acc -> if f x acc == LT then x else acc) x xs

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
