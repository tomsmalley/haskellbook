import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Identity
newtype Identity a = Identity a deriving (Eq, Ord, Show)
instance Functor Identity where
    fmap f (Identity a) = Identity (f a)
instance Foldable Identity where
    foldMap f (Identity a) = f a
instance Traversable Identity where
    traverse f (Identity a) = Identity <$> f a
instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary
instance Eq a => EqProp (Identity a) where (=-=) = eq

-- Constant
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)
instance Functor (Constant a) where
    fmap _ (Constant a) = Constant a
instance Foldable (Constant a) where
    foldMap _ _ = mempty
instance Traversable (Constant a) where
    traverse _ (Constant a) = pure $ Constant a
instance Arbitrary a => Arbitrary (Constant a b) where
    arbitrary = Constant <$> arbitrary
instance Eq a => EqProp (Constant a b) where (=-=) = eq

-- Maybe
data Optional a = Nada | Yep a deriving (Eq, Show)
instance Functor Optional where
    fmap _ Nada = Nada
    fmap f (Yep a) = Yep $ f a
instance Foldable Optional where
    foldMap _ Nada = mempty
    foldMap f (Yep a) = f a
instance Traversable Optional where
    traverse _ Nada = pure Nada
    traverse f (Yep a) = Yep <$> f a
instance Arbitrary a => Arbitrary (Optional a) where
    arbitrary = frequency [(1, pure Nada), (4, Yep <$> arbitrary)]
instance Eq a => EqProp (Optional a) where (=-=) = eq

-- List
data List a = Nil | Cons a (List a) deriving (Eq, Show)
instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a as) = Cons (f a) (fmap f as)
instance Foldable List where
    foldMap _ Nil = mempty
    foldMap f (Cons a as) = f a `mappend` foldMap f as
instance Traversable List where
    traverse _ Nil = pure Nil
    traverse f (Cons a as) = Cons <$> f a <*> traverse f as
instance Arbitrary a => Arbitrary (List a) where
    arbitrary = frequency [(1, pure Nil), (4, Cons <$> arbitrary <*> arbitrary)]
instance Eq a => EqProp (List a) where (=-=) = eq

-- Three
data Three a b c = Three a b c deriving (Eq, Show)
instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)
instance Foldable (Three a b) where
    foldMap f (Three a b c) = f c
instance Traversable (Three a b) where
    traverse f (Three a b c) = (Three a b) <$> f c
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary
instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

-- Three'
data Three' a b = Three' a b b deriving (Eq, Show)
instance Functor (Three' a) where
    fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)
instance Foldable (Three' a) where
    foldMap f (Three' a b1 b2) = f b1 `mappend` f b2
instance Traversable (Three' a) where
    traverse f (Three' a b1 b2) = (Three' a) <$> f b1 <*> f b2
instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary
instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq

-- S
data S n a = S (n a) a deriving (Eq, Show)
instance Functor n => Functor (S n) where
    fmap f (S na a) = S (fmap f na) (f a)
instance Foldable n => Foldable (S n) where
    foldMap f (S na a) = foldMap f na `mappend` f a
instance Traversable n => Traversable (S n) where
    traverse f (S na a) = S <$> traverse f na <*> f a
instance (Arbitrary a, Arbitrary (n a)) => Arbitrary (S n a) where
    arbitrary = S <$> arbitrary <*> arbitrary
instance (Eq a, Eq (n a)) => EqProp (S n a) where (=-=) = eq

-- Tree
data Tree a = Empty
            | Leaf a
            | Node (Tree a) a (Tree a)
            deriving (Eq, Show)
instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Leaf a) = Leaf $ f a
    fmap f (Node t1 a t2) = Node (fmap f t1) (f a) (fmap f t2)
instance Foldable Tree where
    foldMap _ Empty = mempty
    foldMap f (Leaf a) = f a
    foldMap f (Node t1 a t2) = foldMap f t1 `mappend` f a `mappend` foldMap f t2
    foldr _ b Empty = b
    foldr f b (Leaf a) = f a b
    -- different orders
    --    foldr f b (Node t1 a t2) = foldr f (foldr f (f a b) t1) t2
    foldr f b (Node t1 a t2) = f a (foldr f (foldr f b t2) t1)
instance Traversable Tree where
    traverse _ Empty = pure Empty
    traverse f (Leaf a) = Leaf <$> f a
    traverse f (Node t1 a t2) = Node <$> traverse f t1 <*> f a <*> traverse f t2
instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = frequency [ (1, pure Empty)
                          , (1, Leaf <$> arbitrary)
                          , (1, Node <$> arbitrary <*> arbitrary <*> arbitrary)
                          ]
instance Eq a => EqProp (Tree a) where (=-=) = eq

testBlock :: String -> IO ()
testBlock s = do
    putStrLn ""
    putStrLn $ "*********** " ++ s

main = do
    quickBatch (traversable (undefined :: Identity (Int, String, [Int])))
    quickBatch (traversable (undefined :: Constant Int (Int, String, [Int])))
    quickBatch (traversable (undefined :: Optional (Int, String, [Int])))
    quickBatch (traversable (undefined :: List (Int, String, [Int])))
    quickBatch (traversable (undefined :: Three Int Int (Int, String, [Int])))
    quickBatch (traversable (undefined :: Three' Int (Int, String, [Int])))
    testBlock "S"
    quickBatch (functor (undefined :: S Maybe (Int, String, [Int])))
    quickBatch (traversable (undefined :: S Maybe (Int, String, [Int])))
    testBlock "Tree"
    quickBatch (functor (undefined :: Tree (Int, String, [Int])))
    quickBatch (traversable (undefined :: Tree (Int, String, [Int])))
