module Validation where

import Test.QuickCheck (Arbitrary, arbitrary, elements)
import Test.QuickCheck.Checkers (quickBatch, EqProp, (=-=), eq)
import Test.QuickCheck.Classes (applicative)

data Validation e a = Failure e
                    | Success a
                    deriving (Eq, Show)

instance Functor (Validation e) where
    fmap f (Failure e) = Failure e
    fmap f (Success a) = Success $ f a

instance Monoid e => Applicative (Validation e) where
    pure = Success
    Success f <*> Success x = Success $ f x
    Failure a <*> Failure b = Failure $ a `mappend` b
    Failure a <*> _ = Failure a
    _ <*> Failure a = Failure a

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
    arbitrary = do
        e <- arbitrary
        a <- arbitrary
        elements [Success a, Failure e]

instance (Eq a, Eq e) => EqProp (Validation e a) where (=-=) = eq

main :: IO ()
main = do
    quickBatch $ applicative (undefined :: Validation String (Char, Int, Float))
