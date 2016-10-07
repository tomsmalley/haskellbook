module InstancesOfFunc where

import Test.QuickCheck
import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) =>
                     (a -> b)
                  -> (b -> c)
                  -> f a
                  -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) =>
                      f a
                   -> Fun a b
                   -> Fun b c
                   -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
    (fmap (g . f) x) == (fmap g . fmap f $ x)

newtype Identity a = Identity a
  deriving (Eq, Show)
instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return (Identity a)
instance Functor Identity where
    fmap f (Identity a) = Identity $ f a
type IntToInt = Fun Int Int
type IdentFC = Identity Int -> IntToInt -> IntToInt -> Bool

data Pair a = Pair a a
  deriving (Eq, Show)
instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return (Pair a b)
instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)
type PairFC = Pair Int -> IntToInt -> IntToInt -> Bool

data Two a b = Two a b
  deriving (Eq, Show)
instance (Arbitrary a, Arbitrary b) =>
    Arbitrary (Two a b) where
        arbitrary = do
            a <- arbitrary
            b <- arbitrary
            return (Two a b)
instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)
type TwoFC = Two Int String -> Fun String Char -> Fun Char Float -> Bool

data Three a b c = Three a b c
  deriving (Eq, Show)
instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
    Arbitrary (Three a b c) where
        arbitrary = do
            a <- arbitrary
            b <- arbitrary
            c <- arbitrary
            return (Three a b c)
instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b $ f c
type ThreeFC = Three Int Double String -> Fun String Char -> Fun Char Float -> Bool

data Three' a b = Three' a b b
  deriving (Eq, Show)
instance (Arbitrary a, Arbitrary b) =>
    Arbitrary (Three' a b) where
        arbitrary = do
            a <- arbitrary
            b0 <- arbitrary
            b1 <- arbitrary
            return (Three' a b0 b1)
instance Functor (Three' a) where
    fmap f (Three' a b0 b1) = Three' a (f b0) (f b1)
type ThreePFC = Three' Int String -> Fun String Char -> Fun Char Float -> Bool

data Four a b c d = Four a b c d
  deriving (Eq, Show)
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
    Arbitrary (Four a b c d) where
        arbitrary = do
            a <- arbitrary
            b <- arbitrary
            c <- arbitrary
            d <- arbitrary
            return (Four a b c d)
instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)
type FourFC = Four Int Char Double String -> Fun String Char -> Fun Char Float -> Bool

data Four' a b = Four' a a a b
  deriving (Eq, Show)
instance (Arbitrary a, Arbitrary b) =>
    Arbitrary (Four' a b) where
        arbitrary = do
            a0 <- arbitrary
            a1 <- arbitrary
            a2 <- arbitrary
            b <- arbitrary
            return (Four' a0 a1 a2 b)
instance Functor (Four' a) where
    fmap f (Four' a0 a1 a2 b) = Four' a0 a1 a2 (f b)
type FourPFC = Four' Int String -> Fun String Char -> Fun Char Float -> Bool

-- No instance for functor since it requires :k * -> * but Trivial is :k *
data Trivial = Trivial

main :: IO ()
main = do
    putStrLn "Identity"
    quickCheck $ \x -> functorIdentity (x :: Identity Int)
    quickCheck (functorCompose' :: IdentFC)
    putStrLn "Pair"
    quickCheck $ \x -> functorIdentity (x :: Pair Int)
    quickCheck (functorCompose' :: PairFC)
    putStrLn "Two"
    quickCheck $ \x -> functorIdentity (x :: Two Int String)
    quickCheck (functorCompose' :: TwoFC)
    putStrLn "Three"
    quickCheck $ \x -> functorIdentity (x :: Three Int String Double)
    quickCheck (functorCompose' :: ThreeFC)
    putStrLn "Three'"
    quickCheck $ \x -> functorIdentity (x :: Three' Int String)
    quickCheck (functorCompose' :: ThreePFC)
    putStrLn "Four"
    quickCheck $ \x -> functorIdentity (x :: Four Int String Double Char)
    quickCheck (functorCompose' :: FourFC)
    putStrLn "Four'"
    quickCheck $ \x -> functorIdentity (x :: Four' Int String)
    quickCheck (functorCompose' :: FourPFC)
