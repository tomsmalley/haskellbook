{-# LANGUAGE FlexibleInstances #-}

module Possibly where

import GHC.Arr

data Possibly a = LolNope
                | Yeppers a
                deriving (Eq, Show)

instance Functor Possibly where
    fmap f LolNope = LolNope
    fmap f (Yeppers a) = Yeppers $ f a

data Sum a b = First a
             | Second b
             deriving (Eq, Show)

instance Functor (Sum a) where
    fmap f (First a) = First a
    fmap f (Second b) = Second $ f b

data BoolAndSomethingElse a = False' a | True' a
    deriving (Eq, Show)

instance Functor BoolAndSomethingElse where
    fmap f (True' a) = True' $ f a
    fmap f (False' a) = False' $ f a

data BoolAndMaybeSomethingElse a = Falsish
                                 | Truish a
                                 deriving (Eq, Show)

instance Functor BoolAndMaybeSomethingElse where
    fmap _ Falsish = Falsish
    fmap f (Truish a) = Truish $ f a

newtype Mu f = InF { outF :: f (Mu f) }

data D = D (Array Word Word) Int Int

data Sum' b a = First' a
             | Second' b
instance Functor (Sum' e) where
    fmap f (First' a) = First' (f a)
    fmap f (Second' b) = Second' b

data Company a c b = DeepBlue a c
                   | Something b
instance Functor (Company e e') where
    fmap f (Something b) = Something $ f b
    fmap _ (DeepBlue a c) = DeepBlue a c

data More b a = L a b a
              | R b a b
              deriving (Eq, Show)
instance Functor (More x) where
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R b (f a) b'
moreTest :: IO ()
moreTest = do
    print $ fmap (+1) (L 1 2 3) == L 2 2 4
    print $ fmap (+1) (R 1 2 3) == R 1 3 3

data Quant a b = Finance
               | Desk a
               | Bloor b
               deriving (Eq, Show)
instance Functor (Quant a) where
    fmap f (Bloor b) = Bloor $ f b
    fmap _ Finance = Finance
    fmap _ (Desk a) = Desk a

data K a b = K a
instance Functor (K a) where
    fmap _ (K a) = K a

newtype Flip f a b = Flip (f b a)
  deriving (Eq, Show)
instance Functor (Flip K a) where
    fmap f (Flip (K a)) = Flip $ K $ f a

data EvilGoateeConst a b = GoatyConst b
instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst b) = GoatyConst $ f b

data LiftItOut f a = LiftItOut (f a)
instance Functor f => Functor (LiftItOut f) where
    fmap f (LiftItOut fa) = LiftItOut $ fmap f fa

data Parappa f g a = DaWrappa (f a) (g a)
instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
instance Functor g => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

data Notorious g o a t = Notorious (g o) (g a) (g t)
instance Functor g => Functor (Notorious g o a) where
    fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

data List a = Nil
            | Cons a (List a)
instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a as) = Cons (f a) $ fmap f as

data GoatLord a = NoGoat
                | OneGoat a
                | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
instance Functor GoatLord where
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat a) = OneGoat $ f a
    fmap f (MoreGoats a b c) = MoreGoats (fmap f a) (fmap f b) (fmap f c)

data TalkToMe a = Halt
                | Print String a
                | Read (String -> a)
instance Functor TalkToMe where
    fmap _ Halt = Halt
    fmap f (Print s a) = Print s (f a)
    fmap f (Read g) = Read (f . g)
