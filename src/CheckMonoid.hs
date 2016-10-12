module CheckMonoid where

import Control.Monad
import Data.Monoid hiding ((<>))
import Data.Semigroup
import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)
instance Semigroup Trivial where
    _ <> _ = Trivial
instance Monoid Trivial where
    mempty = Trivial
    mappend = (<>)
instance Arbitrary Trivial where
    arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)
monoidLeftIdentity :: (Eq a, Monoid a, Semigroup a) => a -> Bool
monoidLeftIdentity x = x == (x <> mempty)
monoidRightIdentity :: (Eq a, Monoid a, Semigroup a) => a -> Bool
monoidRightIdentity x = x == (mempty <> x)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-- 2
newtype Identity a = Identity a
  deriving (Eq, Show)
instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return $ Identity a
instance Semigroup a => Semigroup (Identity a) where
    (Identity x) <> (Identity y) = Identity (x <> y)
instance (Semigroup a, Monoid a) => Monoid (Identity a) where
    mempty = Identity mempty
    mappend = (<>)
type IdentityAssoc a = Identity a -> Identity a -> Identity a -> Bool

-- 3
data Two a b = Two a b
  deriving (Eq, Show)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Two a b
instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two x1 y1) <> (Two x2 y2) = Two (x1 <> x2) (y1 <> y2)
instance (Semigroup a, Semigroup b, Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty
    mappend = (<>)
type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool


-- 4
data Three a b c = Three a b c
  deriving (Eq, Show)
instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
    Arbitrary (Three a b c) where
        arbitrary = do
            a <- arbitrary
            b <- arbitrary
            c <- arbitrary
            return $ Three a b c
instance (Semigroup a, Semigroup b, Semigroup c)
    => Semigroup (Three a b c) where
        (Three x1 y1 z1) <> (Three x2 y2 z2) = Three (x1 <> x2) (y1 <> y2) (z1 <> z2)
type ThreeAssoc a b c = Three a b c -> Three a b c -> Three a b c -> Bool

-- 5
data Four a b c d = Four a b c d
  deriving (Eq, Show)
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
    Arbitrary (Four a b c d) where
        arbitrary = do
            a <- arbitrary
            b <- arbitrary
            c <- arbitrary
            d <- arbitrary
            return $ Four a b c d
instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d)
    => Semigroup (Four a b c d) where
        (Four a1 b1 c1 d1) <> (Four a2 b2 c2 d2) = Four (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2)
type FourAssoc a b c d = Four a b c d -> Four a b c d -> Four a b c d -> Bool

-- 6
newtype BoolConj = BoolConj Bool
  deriving (Eq, Show)
instance Arbitrary BoolConj where
    arbitrary = elements [BoolConj True, BoolConj False]
instance Semigroup BoolConj where
    (BoolConj True) <> (BoolConj True) = BoolConj True
    _ <> _ = BoolConj False
instance Monoid BoolConj where
    mappend = (<>)
    mempty = BoolConj True
type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- 7
newtype BoolDisj = BoolDisj Bool
  deriving (Eq, Show)
instance Arbitrary BoolDisj where
    arbitrary = elements [BoolDisj True, BoolDisj False]
instance Semigroup BoolDisj where
    (BoolDisj False) <> (BoolDisj False) = BoolDisj False
    _ <> _ = BoolDisj True
instance Monoid BoolDisj where
    mappend = (<>)
    mempty = BoolDisj False
type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- 8
data Or a b = Fst a | Snd b
  deriving (Eq, Show)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [Fst a, Snd b]
instance Semigroup (Or a b) where
    (Snd b) <> _ = Snd b
    _ <> (Snd b) = Snd b
    _ <> (Fst a) = Fst a
type OrAssoc a b = Or a b -> Or a b -> Or a b -> Bool

-- 9
newtype Combine a b = Combine { unCombine :: (a -> b) }
instance (Arbitrary a, Arbitrary b)
    => Arbitrary (Combine a b) where
        arbitrary = undefined
instance Semigroup b => Semigroup (Combine a b) where
    Combine f <> Combine g = Combine ( f <> g )
instance (Monoid b, Semigroup b) => Monoid (Combine a b) where
    mappend = (<>)
    mempty = Combine mempty

-- 10
newtype Comp a = Comp { unComp :: (a -> a) }
instance Arbitrary (Comp a) where
    arbitrary = undefined
instance (Semigroup a) => Semigroup (Comp a) where
    Comp f <> Comp g = Comp (f . g)
instance (Monoid a, Semigroup a) => Monoid (Comp a) where
    mempty = Comp mempty
    mappend = (<>)

-- 11
data Validation a b = VFailure a | VSuccess b
  deriving (Eq, Show)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [VFailure a, VSuccess b]
instance Semigroup b => Semigroup (Validation a b) where
    VSuccess x <> VSuccess y = VSuccess $ x <> y
    VSuccess x <> _ = VSuccess x
    _ <> VSuccess x = VSuccess x
    _ <> VFailure x = VFailure x
type ValidAssoc a b = Validation a b -> Validation a b -> Validation a b -> Bool

-- 12
newtype AccumulateRight a b = AccumulateRight (Validation a b)
  deriving (Eq, Show)
instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [AccumulateRight (VFailure a), AccumulateRight (VSuccess b)]
instance Semigroup b => Semigroup (AccumulateRight a b) where
    AccumulateRight (VSuccess x) <> AccumulateRight (VSuccess y) = AccumulateRight (VSuccess (x <> y))
    AccumulateRight (VSuccess x) <> _ = AccumulateRight (VSuccess x)
    _ <> AccumulateRight (VSuccess x) = AccumulateRight (VSuccess x)
    _ <> AccumulateRight (VFailure x) = AccumulateRight (VFailure x)
type RightAssoc a b = AccumulateRight a b -> AccumulateRight a b -> AccumulateRight a b -> Bool

-- 13
newtype AccumulateBoth a b = AccumulateBoth (Validation a b)
  deriving (Eq, Show)
instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [AccumulateBoth (VFailure a), AccumulateBoth (VSuccess b)]
instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
    AccumulateBoth (VSuccess x) <> AccumulateBoth (VSuccess y) = AccumulateBoth (VSuccess (x <> y))
    AccumulateBoth (VFailure x) <> AccumulateBoth (VFailure y) = AccumulateBoth (VFailure (x <> y))
    AccumulateBoth (VSuccess x) <> _ = AccumulateBoth (VSuccess x)
    _ <> AccumulateBoth (VSuccess x) = AccumulateBoth (VSuccess x)
type BothAssoc a b = AccumulateBoth a b -> AccumulateBoth a b -> AccumulateBoth a b -> Bool

newtype Mem s a = Mem { runMem :: s -> (a, s) }
instance (Semigroup a) => Semigroup (Mem s a) where
    --Mem f <> Mem g = Mem (\s -> ((fst $ f s) <> (fst $ g s), snd . g . snd $ f s))
    Mem f <> Mem g = Mem $ \s -> let (a, b) = f s
                                     (c, d) = g b
                                  in (a <> c, d)
instance (Monoid a, Semigroup a) => Monoid (Mem s a) where
    mempty = Mem $ \s -> (mempty, s)
    mappend = (<>)
f' :: Mem Int String
f' = Mem $ \s -> ("hi", s + 1)

main :: IO ()
main = do
    print $ runMem (f' <> mempty) 0
    print $ runMem (mempty <> f') 0
    print $ (runMem mempty 0 :: (String, Int))
    print $ runMem (f' <> mempty) 0 == runMem f' 0
    print $ runMem (mempty <> f') 0 == runMem f' 0
    putStrLn "Trivial"
    quickCheck (semigroupAssoc :: TrivialAssoc)
    quickCheck (monoidLeftIdentity :: Trivial -> Bool)
    quickCheck (monoidRightIdentity :: Trivial -> Bool)
    putStrLn "Identity"
    quickCheck (semigroupAssoc :: (IdentityAssoc String))
    quickCheck (monoidLeftIdentity :: Identity String -> Bool)
    quickCheck (monoidRightIdentity :: Identity String -> Bool)
    putStrLn "Two"
    quickCheck (semigroupAssoc :: (TwoAssoc [Int] String))
    quickCheck (monoidLeftIdentity :: Two [Int] String -> Bool)
    quickCheck (monoidRightIdentity :: Two [Int] String -> Bool)
    putStrLn "Three"
    quickCheck (semigroupAssoc :: (ThreeAssoc String [Int] [Float]))
    putStrLn "Four"
    quickCheck (semigroupAssoc :: (FourAssoc String String String String))
    putStrLn "BoolConj"
    quickCheck (semigroupAssoc :: BoolConjAssoc)
    quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
    quickCheck (monoidRightIdentity :: BoolConj -> Bool)
    putStrLn "BoolDisj"
    quickCheck (semigroupAssoc :: BoolDisjAssoc)
    quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
    quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
    quickCheck (semigroupAssoc :: (OrAssoc String [Int]))
    --quickCheck (semigroupAssoc :: (CombineAssoc String [Float]))
    --quickCheck (semigroupAssoc :: Comp Int)
    quickCheck (semigroupAssoc :: ValidAssoc Int String)
    quickCheck (semigroupAssoc :: RightAssoc Int String)
    quickCheck (semigroupAssoc :: BothAssoc String String)
