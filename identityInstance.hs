import Data.Monoid

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity
    (Identity f) <*> (Identity a) = Identity (f a)


newtype Constant a b = Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
    pure = Constant . mempty
    (Constant f) <*> (Constant a) = Constant (f <> a)

validateLength :: Int -> String -> Maybe String
validateLength maxLen s = if (length s) > maxLen
                          then Nothing
                          else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a

data Person = Person Name Address
  deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a = Person <$> mkName n <*> mkAddress a

justIf :: (a -> Bool) -> a -> Maybe a
justIf p x = if p x then Just x else Nothing

data Cow = Cow { name :: String
               , age :: Int
               , weight :: Int
               } deriving (Eq, Show)
noEmpty :: String -> Maybe String
noEmpty = justIf (not . null)
noNegative :: Int -> Maybe Int
noNegative = justIf (>0)

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString n a w = Cow <$> noEmpty n <*> noNegative a <*> noNegative w

fixer = const <$> Just "Hello" <*> pure "World"
upper = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1,2,3]
