module DetermineTheType where

i :: a -> a
i x = x

c :: a -> b -> a
c x _ = x

c' :: a -> b -> b
c' _ x = x

-- tail
r1 :: [a] -> [a]
r1 (_:xs) = xs

-- reverse
r2 :: [a] -> [a]
r2 [] = []
r2 (x:xs) = (r2 xs) ++ [x]

-- function composition
co :: (b -> c) -> (a -> b) -> (a -> c)
co f g = f . g

a :: (a -> c) -> a -> a
a _ x = x

a' :: (a -> b) -> a -> b
a' f x = f x
