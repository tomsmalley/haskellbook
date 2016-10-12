module ApplyTimes where

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f (applyTimes (n-1) f b)

{-
applyTimes 5 (+1) 5
    = (+1) (applyTimes (4) (+1) 5)
    = (+1) (applyTimes (3) (+1) 5)
    = (+1) (applyTimes (2) (+1) 5)
    = (+1) (applyTimes (1) (+1) 5)
    = (+1) (applyTimes (0) (+1) 5)
    = 5
-}

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)

data DividedResult a = Result a
                     | DividedByZero
                     deriving (Eq, Show)

dividedBy' :: Integral a => a -> a -> DividedResult a
dividedBy' _ 0 = DividedByZero
dividedBy' num denom
    | (num < 0) /= (denom < 0) = Result $ negate $ start num denom
    | otherwise = Result $ start num denom
    where start n d = go (abs n) (abs d) 0
          go n d count
            | n < d = count
            | otherwise = go (n - d) d (count + 1)

mc91 :: (Integral a) => a -> a
mc91 n | n > 100 = n - 10
       | otherwise = mc91 . mc91 $ n + 11

{-
dividedBy 15 2 = go 15 2 0
    -> go 13 2 1
    -> go 11 2 2
    -> go 9  2 3
    -> go 7  2 4
    -> go 5  2 5
    -> go 3  2 6
    -> go 1  2 7
    -> (7, 1)
-}

recSum :: (Eq a, Num a) => a -> a
recSum 0 = 0
recSum n = n + recSum (n - 1)

mult2 :: (Integral a) => a -> a -> a
mult2 _ 0 = 0
mult2 0 _ = 0
mult2 x y = x + mult2 x (y-1)
