module Factorial where

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

factorial' :: Int -> Integer
factorial' = (!!) facs
  where facs = scanl (*) 1 [1..]
