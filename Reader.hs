{-# LANGUAGE InstanceSigs #-}

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
    fmap f (Reader ra) = Reader $ f . ra
instance Applicative (Reader r) where
    pure a = Reader (\r -> a)
    (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    (Reader rab) <*> (Reader ra) = Reader (\r -> rab r (ra r))
--    (Reader rab) <*> (Reader ra) = Reader (\r -> (fmap (rab r) ra) r)
instance Monad (Reader r) where
    return = pure

    (>>=) :: Reader r a        -> (a -> Reader r b) -> Reader r b
    (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb (ra r)) r

ask :: Reader a a
ask = Reader id

newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person { humanName :: HumanName
                     , dogName :: DogName
                     , address :: Address
                     } deriving (Eq, Show)

data Dog = Dog { dogsName :: DogName
               , dogsAddress :: Address
               } deriving (Eq, Show)

getDogReader :: Person -> Dog
getDogReader = runReader $ Dog <$> Reader dogName <*> Reader address

pers :: Person
pers = Person (HumanName "Big Bird") (DogName "Barkey") (Address "Sesame Street")

chris :: Person
chris = Person (HumanName "Chris") (DogName "Papu") (Address "Austin")

getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

getDogRM :: Person -> Dog
getDogRM = do
    name <- dogName
    addy <- address
    return $ Dog name addy

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb

asks :: (r -> a) -> Reader r a
asks = Reader


