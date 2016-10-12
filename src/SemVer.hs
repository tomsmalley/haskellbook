module SemVer where

import Control.Applicative
import Text.Trifecta
import Data.List (intercalate, sort)
import Test.Hspec
import Data.Foldable (toList)
import Text.Read (readMaybe)

data NumberOrString = NOSS String
                    | NOSI Integer
                    deriving Eq
instance Show NumberOrString where
    show (NOSS s) = s
    show (NOSI i) = show i
instance Ord NumberOrString where
    NOSS s `compare` NOSS s' = compare s s'
    NOSI i `compare` NOSI i' = compare i i'
    NOSI _ `compare` NOSS _ = LT
    NOSS _ `compare` NOSI _ = GT

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata deriving Eq
instance Show SemVer where
    show (SemVer a b c p m) = (intercalate "." $ map show [a,b,c]) ++ build ++ metadata
      where build = if null p then "" else "-" ++ (intercalate "." $ map show p)
            metadata = if null m then "" else "+" ++ (intercalate "." $ map show m)
instance Ord SemVer where
    SemVer a b c p _ `compare` SemVer a' b' c' p' _ =
        case compare (a, b, c) (a', b', c') of
            EQ -> preCompare
            _  -> compare (a, b, c) (a', b', c')
        where preCompare | null p && null p' = EQ
                         | null p = GT
                         | null p' = LT
                         | otherwise = compare p p'

parseSemVer :: Parser SemVer
parseSemVer = SemVer <$> integer
                     <*> (char '.' *> integer)
                     <*> (char '.' *> integer)
                     <*> (char '-' *> sepBy1 parseNos (char '.') <|> mempty)
                     <*> (char '+' *> sepBy1 parseNos (char '.') <|> mempty)

parseNos :: Parser NumberOrString
parseNos = NOSS <$> ('0':) <$> try (char '0' *> some alphaNum) -- handles things like 001 correctly
       <|> NOSI <$> try (integer <* notFollowedBy letter)
       <|> NOSS <$> some alphaNum

show1 = SemVer 1 0 0 [NOSS "alpha"] [NOSS "001"]
show2 = SemVer 1 0 0 [] [NOSI 20130313144700]
show3 = SemVer 1 0 0 [NOSS "beta"] [NOSS "exp", NOSS "sha", NOSS "5114f85"]
testorder = [ (SemVer 1 0 0 [NOSS "alpha"] [])
            , (SemVer 1 0 0 [NOSS "alpha", NOSI 1] [])
            , (SemVer 1 0 0 [NOSS "alpha", NOSS "beta"] [])
            , (SemVer 1 0 0 [NOSS "beta"] [])
            , (SemVer 1 0 0 [NOSS "beta", NOSI 2] [])
            , (SemVer 1 0 0 [NOSS "beta", NOSI 11] [])
            , (SemVer 1 0 0 [NOSS "rc", NOSI 1] [])
            , (SemVer 1 0 0 [] [])
            , (SemVer 1 0 1 [] [])
            , (SemVer 1 1 1 [] [])
            , (SemVer 2 0 0 [] [])
            ]

ex1 = parseString parseSemVer mempty "2.1.1"
an1 = SemVer 2 1 1 [] []
ex2 = parseString parseSemVer mempty "1.0.0-x.7.z.92"
an2 = SemVer 1 0 0 [NOSS "x", NOSI 7, NOSS "z", NOSI 92] []
ex3 = parseString parseSemVer mempty "0.0.1-123a.456+test"
an3 = SemVer 0 0 1 [NOSS "123a", NOSI 456] [NOSS "test"]
ex4 = parseString parseSemVer mempty "0.0.1-alpha+001"
an4 = SemVer 0 0 1 [NOSS "alpha"] [NOSS "001"]

main = hspec $ do
    describe "Show testing" $ do
        it "Prints 1.0.0-alpha+001 correctly" $ do
            show show1 `shouldBe` "1.0.0-alpha+001"
        it "Prints 1.0.0+20130313144700 correctly" $ do
            show show2 `shouldBe` "1.0.0+20130313144700"
        it "Prints 1.0.0-beta+exp.sha.5114f85 correctly" $ do
            show show3 `shouldBe` "1.0.0-beta+exp.sha.5114f85"
    describe "Ord testing - book example" $ do
        it "2.1.1 is greater than 2.1.0" $ do
            SemVer 2 1 1 [] [] > SemVer 2 1 0 [] [] `shouldBe` True
    describe "Ord testing - semver.org example" $ do
        it "Should be the correct order" $ do
            testorder `shouldBe` sort (reverse testorder)
    describe "Parse testing - book example" $ do
        it "Parses 2.1.1 correctly" $ do
            toList ex1 `shouldBe` [an1]
        it "Parses 1.0.0-x.7.z.92 correctly" $ do
            toList ex2 `shouldBe` [an2]
    describe "Parse testing - my example" $ do
        it "Parses 0.0.1-123a.456+test correctly" $ do
            toList ex3 `shouldBe` [an3]
        it "Parses 0.0.1-alpha+001 correctly" $ do
            toList ex4 `shouldBe` [an4]
