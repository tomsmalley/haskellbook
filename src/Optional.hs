module Optional where

data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
    mempty = Nada
    mappend (Only a) (Only b) = Only $ mappend a b
    mappend (Only a) Nada = Only a
    mappend Nada (Only a) = Only a
    mappend Nada Nada = Nada
