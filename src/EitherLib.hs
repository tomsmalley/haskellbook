module EitherLib where

lefts' :: [Either a b] -> [a]
lefts' = foldr f []
    where f (Left x) acc = x : acc
          f _ acc = acc

rights' :: [Either a b] -> [b]
rights' = foldr f []
    where f (Right x) acc = x : acc
          f _ acc = acc

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr f ([], [])
    where f (Left a) (as, bs) = (a:as, bs)
          f (Right b) (as, bs) = (as, b:bs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right b) = Just $ f b

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ f (Right b) = f b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (\x -> Nothing) (Just . f)
