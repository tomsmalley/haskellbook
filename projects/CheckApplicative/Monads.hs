module Monads where

import Control.Monad
import Test.QuickCheck (Arbitrary, arbitrary, elements, frequency)
import Test.QuickCheck.Checkers (quickBatch, EqProp, (=-=), eq)
import Test.QuickCheck.Classes (functor, applicative, monad)

data CountMe a = CountMe Integer a deriving(Eq, Show)

instance Functor CountMe where
    fmap f (CountMe i a) = CountMe i (f a)

instance Applicative CountMe where
    pure = CountMe 0
    CountMe n f <*> CountMe n' a = CountMe (n + n') (f a)

instance Monad CountMe where
    return = pure
    CountMe n a >>= f = let CountMe n' b = f a
                         in CountMe (n + n') b

instance Arbitrary a => Arbitrary (CountMe a) where
    arbitrary = CountMe <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMe a) where (=-=) = eq

-- | Nope monad

data Nope a = NopeDotJpg deriving (Eq, Show)
instance Functor Nope where
    fmap _ NopeDotJpg = NopeDotJpg
instance Applicative Nope where
    pure x = NopeDotJpg
    _ <*> _ = NopeDotJpg
instance Monad Nope where
    _ >>= _ = NopeDotJpg
instance Arbitrary (Nope a) where
    arbitrary = pure NopeDotJpg
instance EqProp (Nope a) where (=-=) = eq

-- | PhhhbbtttEither

data PhhhbbtttEither b a = Lpeft a
                         | Rpight b
                         deriving (Eq, Show)
instance Functor (PhhhbbtttEither b) where
    fmap _ (Rpight b) = Rpight b
    fmap f (Lpeft a) = Lpeft $ f a
instance Applicative (PhhhbbtttEither b) where
    pure = Lpeft
    Rpight b <*> _ = Rpight b
    _ <*> Rpight b = Rpight b
    Lpeft f <*> Lpeft a = Lpeft $ f a
instance Monad (PhhhbbtttEither b) where
    Rpight b >>= _ = Rpight b
    Lpeft a >>= f = f a
instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [Lpeft a, Rpight b]
instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where (=-=) = eq

-- | Identity
newtype Identity a = Identity a deriving (Eq, Ord, Show)
instance Functor Identity where
    fmap f (Identity a) = Identity $ f a
instance Applicative Identity where
    pure = Identity
    Identity f <*> Identity a = Identity $ f a
instance Monad Identity where
    Identity a >>= f = f a
instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary
instance Eq a => EqProp (Identity a) where (=-=) = eq

data List a = Nil
            | Cons a (List a)
            deriving (Eq, Show)
instance Monoid (List a) where
    mempty = Nil
    Nil `mappend` bs = bs
    Cons a as `mappend` bs = Cons a (as `mappend` bs)
instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a as) = Cons (f a) (fmap f as)
instance Applicative List where
    pure a = Cons a Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    Cons f fs <*> as = (fmap f as) `mappend` (fs <*> as)
instance Monad List where
    Nil >>= f = Nil
    Cons a as >>= f = f a `mappend` (as >>= f)
instance Arbitrary a => Arbitrary (List a) where
    arbitrary = do
        a <- arbitrary
        frequency [ (1, return Nil)
                  , (4, Cons a <$> arbitrary)
                  ]
instance (Eq a) => EqProp (List a) where (=-=) = eq

-- | Functions

j :: Monad m => m (m a) -> m a
j = join

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (a:as) f = do
    b <- f a -- :: b
    bs <- meh as f -- :: [b]
    return $ b:bs

flipType :: Monad m => [m a] -> m [a]
flipType = flip meh id

main = do
    putStrLn "CountMe"
    let countMeT = undefined :: CountMe (Int, String, Double)
    quickBatch $ functor countMeT
    quickBatch $ applicative countMeT
    quickBatch $ monad countMeT
    putStrLn "Nope"
    let nopeT = undefined :: Nope (Int, String, Double)
    quickBatch $ functor nopeT
    quickBatch $ applicative nopeT
    quickBatch $ monad nopeT
    putStrLn "PhhhbbtttEither"
    let phhhbbtttEitherT = undefined :: PhhhbbtttEither Int (Int, String, Double)
    quickBatch $ functor phhhbbtttEitherT
    quickBatch $ applicative phhhbbtttEitherT
    quickBatch $ monad phhhbbtttEitherT
    putStrLn "Identity"
    let identityT = undefined :: Identity (Int, String, Double)
    quickBatch $ functor identityT
    quickBatch $ applicative identityT
    quickBatch $ monad identityT
    putStrLn "List"
    let listT = undefined :: List (Int, String, Double)
    quickBatch $ functor listT
    quickBatch $ applicative listT
    quickBatch $ monad listT
