import Data.Maybe (fromJust)

data Nat = Zero | Succ Nat
    deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat int
    | int < 0 = Nothing
    | otherwise = Just $ go int
    where go x = if x == 0 then Zero else Succ . go $ x - 1
