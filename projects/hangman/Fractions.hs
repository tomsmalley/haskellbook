{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Text.Fractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta
import Text.RawString.QQ

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
    numerator <- decimal
    char '/'
    denominator <- decimal
    case denominator of
         0 -> fail "Can't divide by zero"
         _ -> return (numerator % denominator)

parse123 :: Parser Integer
parse123 = do
    int <- integer
    eof
    return int

type NumberOrString = Either Integer String
parseNos :: Parser NumberOrString
parseNos = do
    skipMany (oneOf "\n")
    v <- (Left <$> integer) <|> (Right <$> some letter)
    skipMany (oneOf "\n")
    return v

eitherOr :: String
eitherOr = [r|
123
abc
456
def
|]

parseTry :: Parser (Either Rational Double)
parseTry = try (Left <$> parseFraction) <|> Right <$> double

main = do
    print $ parseString parseFraction mempty shouldWork
    print $ parseString parseFraction mempty shouldAlsoWork
    print $ parseString parseFraction mempty alsoBad
    print $ parseString parseFraction mempty badFraction
    print $ parseString parse123 mempty "123"
    print $ parseString parse123 mempty "123abc"
    print $ parseString (some parseNos) mempty eitherOr
    print "----------------------------------"
    print $ parseString parseTry mempty "75"
    print $ parseString parseTry mempty "1.213"
    print $ parseString parseTry mempty "1/0"
    print $ parseString parseTry mempty "1/2"
