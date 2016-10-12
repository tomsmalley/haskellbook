module Apl1 where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a = Nil
            | Cons a (List a)
            deriving (Eq, Show)

instance Eq a => EqProp (List a) where (=-=) = eq

instance Monoid (List a) where
    mempty = Nil
    mappend = append

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
    pure x = Cons x Nil
    Nil <*> _ = Nil
    Cons f fs <*> xs = (fmap f xs) <> (fs <*> xs)

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = do
        a <- arbitrary
        frequency [ (1, return Nil)
                  , (4, Cons a <$> arbitrary)
                  ]

newtype ZipList' a = ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
    xs =-= ys = xs' `eq` ys'
      where xs' = let (ZipList' l) = xs
                   in take' 3000 l
            ys' = let (ZipList' l) = ys
                   in take' 3000 l

instance Functor ZipList' where
    fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
    -- pure :: forall a. a -> ZipList' a
    pure = ZipList' . pure
    ZipList' fs <*> ZipList' xs = ZipList' $ zipList fs xs

instance Arbitrary a => Arbitrary (ZipList' a) where
    arbitrary = ZipList' <$> arbitrary

zipList :: List (a -> b) -> List a -> List b
zipList Nil _ = Nil
zipList _ Nil = Nil
zipList (Cons f fs) (Cons x xs) = Cons (f x) (zipList fs xs)

functions = Cons (+1) (Cons (*2) Nil)
values = Cons 1 (Cons 2 Nil)

z = ZipList' $ Cons (+9) $ Cons (*2) $ Cons (+8) Nil
z' = ZipList' $ Cons 1 $ Cons 2 $ Cons 3 Nil
z'' = ZipList' . foldr Cons Nil $ repeat 1

take' :: Int -> List a -> List a
take' = undefined

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f = concat' . fmap f

instance Monoid a => Monoid (ZipList a) where
    mempty = pure mempty
    mappend = liftA2 mappend

instance Arbitrary a => Arbitrary (ZipList a) where
    arbitrary = ZipList <$> arbitrary

instance Arbitrary a => Arbitrary (Sum a) where
    arbitrary = Sum <$> arbitrary

instance Eq a => EqProp (ZipList a) where (=-=) = eq

main :: IO ()
main = do
    print $ fmap (\x -> Cons x (Cons 9 Nil)) (Cons 1 (Cons 2 (Cons 3 Nil)))
    print $ flatMap (\x -> Cons x (Cons 9 Nil)) (Cons 1 (Cons 2 (Cons 3 Nil)))
    quickBatch $ applicative $ Cons ('a', 2 :: Int, "hell") Nil
