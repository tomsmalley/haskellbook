module PhoneNumberParser where

import Text.Trifecta
import Test.Hspec
import Control.Applicative
import Data.Foldable (toList)

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
    PhoneNumber NumberingPlanArea Exchange LineNumber
    deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = do
    skipOptional (symbol "1-")
    area <- parens (parseSeq 3) <|> parseSeq 3
    skipMany (oneOf "- ")
    exch <- parseSeq 3
    skipMany (oneOf "- ")
    line <- parseSeq 4
    return $ PhoneNumber area exch line

parseSeq :: Int -> Parser Int
parseSeq n = read <$> count n digit

main = hspec $ do
    describe "Phone number parser" $ do
        it "Can parse 123-456-7890" $ do
            toList (parseString parsePhone mempty "123-456-7890")
                `shouldBe` [PhoneNumber 123 456 7890]
        it "Can parse 1234567890" $ do
            toList (parseString parsePhone mempty "1234567890")
                `shouldBe` [PhoneNumber 123 456 7890]
        it "Can parse (123) 456-7890" $ do
            toList (parseString parsePhone mempty "(123) 456-7890")
                `shouldBe` [PhoneNumber 123 456 7890]
        it "Can parse 1-123-456-7890" $ do
            toList (parseString parsePhone mempty "1-123-456-7890")
                `shouldBe` [PhoneNumber 123 456 7890]
