module IntegerParser where

import Text.Trifecta
import Test.Hspec
import Control.Applicative
import Data.Foldable (toList)

parseDigit :: Parser Char
parseDigit = oneOf "0123456789"

base10Integer :: Parser Integer
base10Integer = read <$> some parseDigit

base10Integer' :: Parser Integer
base10Integer' = do
    neg <- optional (char '-')
    case neg of
         Just _ -> negate <$> base10Integer
         Nothing -> base10Integer

base10Integer'' :: Parser Integer
base10Integer'' = char '-' >> negate <$> base10Integer
              <|> base10Integer

main = hspec $ do
    describe "Digit parser" $ do
        it "Parses the first digit of 123" $ do
            toList (parseString parseDigit mempty "123") `shouldBe` ['1']
        it "Fails to parse abc" $ do
            toList (parseString parseDigit mempty "abc") `shouldBe` []
    describe "Integer parser" $ do
        it "Parses the integer out of 123abc" $ do
            toList (parseString base10Integer mempty "123abc") `shouldBe` [123]
        it "Fails to parse abc" $ do
            toList (parseString base10Integer mempty "abc") `shouldBe` []
    describe "Negative integer parser" $ do
        it "Parses the negative integer out of -123abc" $ do
            toList (parseString base10Integer' mempty "-123abc") `shouldBe` [-123]
        it "Parses the negative integer out of -123abc" $ do
            toList (parseString base10Integer'' mempty "-123abc") `shouldBe` [-123]
