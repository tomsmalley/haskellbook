module Addition where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function
import Data.List (sort)
import Data.Char (toUpper)

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
    where go _ status@(_, False) = status
          go y (Nothing, t) = (Just y, t)
          go y (Just x, t) = (Just y, x >= y)

prop_sortHolds :: [Int] -> Bool
prop_sortHolds = listOrdered . sort

qcList :: IO ()
qcList = quickCheck prop_sortHolds

plusAssociative x y z = x + (y + z) == (x + y) + z
plusCommutative x y = x + y == y + x
multAssociative x y z = x * (y * z) == (x * y) * z
multCommutative x y = x * y == y * x

prop_plusAssoc :: Rational -> Rational -> Rational -> Bool
prop_plusAssoc = plusAssociative

prop_plusCommu :: Double -> Double -> Bool
prop_plusCommu = plusCommutative

prop_multAssoc :: Rational -> Rational -> Rational -> Bool
prop_multAssoc = multAssociative

prop_multCommu :: Double -> Double -> Bool
prop_multCommu = multCommutative

qcPlus :: IO ()
qcPlus = do
    quickCheck prop_plusAssoc
    quickCheck prop_plusCommu

qcMult :: IO ()
qcMult = do
    quickCheck prop_multAssoc
    quickCheck prop_multCommu

prop_quotRem :: NonZero Integer -> NonZero Integer -> Bool
prop_quotRem (NonZero x) (NonZero y) = (quot x y) * y + (rem x y) == x
prop_divMod :: NonZero Integer -> NonZero Integer -> Bool
prop_divMod (NonZero x) (NonZero y) = (div x y) * y + (mod x y) == x
qc_quotDiv :: IO ()
qc_quotDiv = do
    quickCheck prop_quotRem
    quickCheck prop_divMod

prop_powerAssoc :: Int -> Int -> Int -> Bool
prop_powerAssoc x y z = (x ^ y) ^ z == x ^ (y ^ z)
prop_powerCommu :: Int -> Int -> Bool
prop_powerCommu x y = x ^ y == y ^ x
qc_power :: IO ()
qc_power = do
    quickCheck prop_powerAssoc
    quickCheck prop_powerCommu

prop_revList :: [Int] -> Bool
prop_revList xs = reverse (reverse xs) == id xs
qc_rev = quickCheck prop_revList

prop_dollar :: Fun Int Int -> Int -> Bool
prop_dollar (Fun _ f) a = f a == (f $ a)

prop_fold :: [Int] -> [Int] -> Bool
prop_fold xs ys = (foldr (:) xs ys) == (++) xs ys
prop_concat :: [[Int]] -> Bool
prop_concat xs = (foldr (++) [] xs) == concat xs

prop_hmm :: Int -> String -> Bool
prop_hmm n xs = length (take n xs) == n

prop_read :: [Float] -> Bool
prop_read x = (read (show x)) == x

prop_square :: Float -> Bool
prop_square x = sqrt x * sqrt x == x

twice f = f . f
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs

prop_cap :: String -> Bool
prop_cap s = (capitalizeWord s == twice capitalizeWord s)
           && (capitalizeWord s == fourTimes capitalizeWord s)
prop_sort :: [Int] -> Bool
prop_sort x = (sort x == twice sort x)
            && (sort x == fourTimes sort x)

data Fool = Fulse | Frue deriving (Eq, Show)
instance Arbitrary Fool where
    arbitrary = elements [Fulse, Fulse, Frue]

mult :: (Integral a) => a -> a -> a
mult _ 0 = 0
mult 0 _ = 0
mult x y = x + mult x (y-1)

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n d count
            | n < d = (count, n)
            | otherwise = go (n - d) d (count + 1)

trivialInt :: Gen Int
trivialInt = elements [1, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
    a <- arbitrary
    b <- arbitrary
    return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
    a <- arbitrary
    b <- arbitrary
    elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
    a <- arbitrary
    elements [Nothing, Just a]

genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
    a <- arbitrary
    frequency [ (1, return Nothing)
              , (3, return (Just a))
              ]

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater

half :: (Fractional a) => a -> a
half x = x / 2
halfIdentity :: (Eq a, Fractional a) => a -> a
halfIdentity = (*2) . half
prop_halfIdentityHolds :: Double -> Bool
prop_halfIdentityHolds x = x == halfIdentity x

qcHalf :: IO ()
qcHalf = quickCheck prop_halfIdentityHolds

main :: IO ()
main = hspec $ do
    describe "Addition" $ do
        it "15 divided by 3 is 5" $ do
            dividedBy 15 3 `shouldBe` (5, 0)
        it "22 divided by 5 is 4 remainder 2" $ do
            dividedBy 22 5 `shouldBe` (4, 2)
        it "5 times 3 is 15" $ do
            mult 5 3 `shouldBe` 15
        it "4 times 0 is 0" $ do
            mult 4 0 `shouldBe` 0
        it "0 times 4 is 0" $ do
            mult 0 4 `shouldBe` 0
        it "x + 1 is always greater than x" $ do
            property $ \x -> x + 1 > (x :: Int)
