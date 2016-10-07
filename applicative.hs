import Control.Applicative
import Data.List (elemIndex)

f x = lookup x [(3, "hello"), (4, "julie"), (5, "kbai")]
g y = lookup y [(7, "sup?"), (8, "chris"), (9, "aloha")]

h z = lookup z [(2, 3), (5, 6), (7, 8)]
--m x = lookup x [(4, 10), (8, 13), (1, 9001)]

added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1,2,3] [4,5,6])

y :: Maybe Integer
y = lookup 3 $ zip [1,2,3] [4,5,6]

z :: Maybe Integer
z = lookup 2 $ zip [1,2,3] [4,5,6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z
-- Alternatively
-- tupled = liftA2 (,) y z

m :: Maybe Int
m = elemIndex 3 [1,2,3,4,5]
n :: Maybe Int
n = elemIndex 4 [1,2,3,4,5]
max' :: Int -> Int -> Int
max' = max
maxed :: Maybe Int
maxed = max' <$> m <*> n

as = [1,2,3]
bs = [4,5,6]
a :: Maybe Integer
a = lookup 3 $ zip as bs
b :: Maybe Integer
b = lookup 2 $ zip as bs
summed :: Maybe Integer
summed = fmap sum $ (,) <$> a <*> b
