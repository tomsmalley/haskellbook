module LetsWriteCode where

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10

tensDigit' :: Integral a => a -> a
tensDigit' x = d
    where xLast = fst $ divMod x 10
          (_, d) = divMod xLast 10

hunsD :: Integral a => a -> a
hunsD x = d
    where xLast = fst $ divMod x 100
          (_, d) = divMod xLast 10

foldBool :: a -> a -> Bool -> a
foldBool x y True  = x
foldBool x y False = y

foldBoolCase :: a -> a -> Bool -> a
foldBoolCase x y b = case b of
                          True -> x
                          False -> y

foldBoolGuard :: a -> a -> Bool -> a
foldBoolGuard x y b
    | b = x
    | otherwise = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (x, y) = (f x, y)
