module MyFoldable where

import Data.Monoid
import Data.Foldable

data Identity a = Identity a

instance Foldable Identity where
    foldr f z (Identity x) = f x z
    foldl f z (Identity x) = f z x
    foldMap f (Identity x) = f x

data Optional a = Yep a | Nada deriving (Eq, Show)

instance Foldable Optional where
    foldr _ z Nada = z
    foldr f z (Yep x) = f x z
    foldl _ z Nada = z
    foldl f z (Yep x) = f z x
    foldMap f Nada = mempty
    foldMap f (Yep a) = f a

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = getAny . foldMap (Any . (== x))

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr f Nothing
    where f x Nothing = Just x
          f x (Just acc) = if x < acc then Just x else Just acc

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr f Nothing
    where f x Nothing = Just x
          f x (Just acc) = if x > acc then Just x else Just acc

null' :: (Foldable t) => t a -> Bool
--null' = foldr (\_ _ -> False) True
null' = getAll . foldMap (All . const False)

length' :: (Foldable t) => t a -> Int
--length' = foldr (\_ acc -> acc + 1) 0
length' = getSum . foldMap (Sum . const 1)

toList' :: (Foldable t) => t a -> [a]
--toList' = foldr (:) []
toList' = foldMap pure

fold' :: (Foldable t, Monoid m) => t m -> m
--fold' = foldr mappend mempty
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (mappend . f) mempty

-- Foldable instances

data Constant a b = Constant a deriving (Eq, Show)
instance Foldable (Constant a) where
    foldMap f (Constant a) = mempty

data Two a b = Two a b deriving (Eq, Show)
instance Foldable (Two a) where
    foldMap f (Two a b) = f b

data Three a b c = Three a b c deriving (Eq, Show)
instance Foldable (Three a b) where
    foldMap f (Three a b c) = f c

data Three' a b = Three' a b b deriving (Eq, Show)
instance Foldable (Three' a) where
    foldMap f (Three' a b1 b2) = f b1 <> f b2

data Four' a b = Four' a b b b deriving (Eq, Show)
instance Foldable (Four' a) where
    foldMap f (Four' a b1 b2 b3) = f b1 <> f b2 <> f b3

--filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF :: (Applicative f, Foldable f, Monoid (f a)) => (a -> Bool) -> f a -> f a
filterF pred = foldMap (\x -> if pred x then pure x else mempty)
