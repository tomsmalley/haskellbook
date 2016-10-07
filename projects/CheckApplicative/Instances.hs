module Instances where

import Test.QuickCheck (Arbitrary, arbitrary, elements)
import Test.QuickCheck.Checkers (quickBatch, EqProp, (=-=), eq)
import Test.QuickCheck.Classes (applicative, functor)

-- | Pair instances

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
    pure x = Pair x x
    Pair f1 f2 <*> Pair x1 x2 = Pair (f1 x1) (f2 x2)

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a) => EqProp (Pair a) where (=-=) = eq

-- | Two instances

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
    pure = Two mempty
    Two a f <*> Two b x = Two (a `mappend` b) (f x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = Two <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where (=-=) = eq

-- | Three instances

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure = Three mempty mempty
    Three a b f <*> Three c d x = Three (a `mappend` c) (b `mappend` d) (f x)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
    Arbitrary (Three a b c) where
        arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

-- | Three' instances

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)

instance Monoid a => Applicative (Three' a) where
    pure x = Three' mempty x x
    Three' a f1 f2 <*> Three' b x1 x2 = Three' (a `mappend` b) (f1 x1) (f2 x2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq

-- | Four instances

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) =>
    Applicative (Four a b c) where
        pure = Four mempty mempty mempty
        Four a1 b1 c1 f <*> Four a2 b2 c2 x = Four (a1 `mappend` a2) (b1 `mappend` b2) (c1 `mappend` c2) (f x)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
    Arbitrary (Four a b c d) where
        arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where (=-=) = eq

-- | Four' instances

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b)

instance (Monoid a) => Applicative (Four' a) where
    pure = Four' mempty mempty mempty
    Four' a1 b1 c1 f <*> Four' a2 b2 c2 x = Four' (a1 `mappend` a2) (b1 `mappend` b2) (c1 `mappend` c2) (f x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where (=-=) = eq

main = do
    putStrLn "Pair tests"
    quickBatch $ functor (undefined :: Pair (Char, Int, Float))
    quickBatch $ applicative (undefined :: Pair (Char, Int, Float))
    putStrLn "Two tests"
    quickBatch $ functor (undefined :: Two (Maybe String) (Char, Int, Float))
    quickBatch $ applicative (undefined :: Two (Maybe String) (Char, Int, Float))
    putStrLn "Three tests"
    quickBatch $ functor (undefined :: Three [Int] (Maybe String) (Char, Int, Float))
    quickBatch $ applicative (undefined :: Three [Int] (Maybe String) (Char, Int, Float))
    putStrLn "Three' tests"
    quickBatch $ functor (undefined :: Three' [Int] (Char, Int, Float))
    quickBatch $ applicative (undefined :: Three' [Int] (Char, Int, Float))
    putStrLn "Four tests"
    quickBatch $ functor (undefined :: Four String [Int] (Maybe String) (Char, Int, Float))
    quickBatch $ applicative (undefined :: Four String [Int] (Maybe String) (Char, Int, Float))
    putStrLn "Four' tests"
    quickBatch $ functor (undefined :: Four' String (Char, Int, Float))
    quickBatch $ applicative (undefined :: Four' String (Char, Int, Float))
