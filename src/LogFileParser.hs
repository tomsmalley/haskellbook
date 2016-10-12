{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module LogFileParser where

import Text.Trifecta
import Test.Hspec
import Data.List (intercalate)
import Control.Applicative
import Data.Foldable (toList)
import Text.RawString.QQ

exampleLog :: String
exampleLog = [r|
-- whee a comment

# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

newtype Log = Log [LogDay] deriving Eq
instance Show Log where
    show (Log ds) = unlines $ map show ds

data LogDay = LogDay Date [Activity] deriving Eq
instance Show LogDay where
    show (LogDay d as) = unlines $ ("# " ++ show d) : map show as

data Activity = Activity Time String deriving Eq
instance Show Activity where
    show (Activity t s) = unwords [show t, s]

data Date = Date Year Month Day deriving Eq
instance Show Date where
    show (Date y m d) = intercalate "-" [show y, showT m, showT d]
type Year = Integer
type Month = Integer
type Day = Integer

data Time = Time Hour Minute deriving Eq
instance Show Time where
    show (Time h m) = showT h ++ ":" ++ showT m
showT :: Integer -> String
showT 0 = "00"
showT x = if x < 10
          then '0' : show x
          else show x
type Hour = Integer
type Minute = Integer

skipLine :: Parser ()
skipLine = skipMany (noneOf "\n") <* skipOptional (oneOf "\n")

comment :: Parser ()
comment = try (skipOptional spaces) <* symbol "--"

timeParser :: Parser Time
timeParser = Time <$> integer <* char ':'
                  <*> integer

activityParser :: Parser Activity
activityParser = do
    time <- timeParser <* spaces
    activity <- manyTill anyChar $ try (comment >> skipLine) <|> (newline >> return ()) <|> eof
    return $ Activity time activity

dateParser :: Parser Date
dateParser = do
    year <- symbol "# " *> integer
    month <- symbol "-" *> integer
    day <- symbol "-" *> integer
    skipMany $ try (comment >> skipLine)
    return $ Date year month day

logDayParser :: Parser LogDay
logDayParser = do
    date <- dateParser
    activities <- many activityParser
    skipLine
    return $ LogDay date activities

logParser :: Parser Log
logParser = do
    skipMany (try (comment >> skipLine) <|> (newline >> return ()))
    days <- some logDayParser
    return $ Log days

timeMidnight = Time 0 0
timeMidnight' = "00:00"
timeOther = Time 8 14
timeOther' = "08:14"
activityTest = Activity timeMidnight "Test"
activityTest' = timeMidnight' ++ " Test"
activityHome = Activity timeOther "Going home"
activityHome' = timeOther' ++ " Going home"
activityHomeWithComment' = timeOther' ++ " Going home -- comment"
date1 = Date 2025 02 05
date1' = "# 2025-02-05"
date2 = Date 2025 02 07
date2' = "# 2025-02-07"
date2WithComment' = "# 2025-02-07 -- haha"
logDay1 = LogDay date1 [activityTest, activityHome]
logDay1' = unlines [date1', activityTest', activityHome']
logDay2 = LogDay date2 [activityHome]
logDay2' = unlines [date2', activityHome']
logTest = Log [logDay1, logDay2]
logTest' = unlines [logDay1', logDay2']
main = hspec $ do
    describe "Show instances" $ do
        describe "Basic data type tests" $ do
            it "Shows times of 0 correctly" $ do
                showT 0 `shouldBe` "00"
            it "Shows times < 10 correctly" $ do
                showT 9 `shouldBe` "09"
            it "Shows times > 10 correctly" $ do
                showT 13 `shouldBe` "13"
        describe "Hour:Minute representations" $ do
            it (unwords ["Shows", timeMidnight', "correctly"]) $ do
                show timeMidnight `shouldBe` timeMidnight'
            it (unwords ["Shows", timeOther',  "correctly"]) $ do
                show timeOther `shouldBe` timeOther'
        describe "Activity representations" $ do
            it "Shows an activity correctly" $ do
                show activityTest `shouldBe` activityTest'
        describe "LogDay representations" $ do
            it "Shows the daily log correctly" $ do
                show logDay1 `shouldBe` logDay1'
        describe "Log representations" $ do
            it "Shows the full log correctly" $ do
                show logTest `shouldBe` logTest'
    describe "Parsing" $ do
        describe "Parse times" $ do
            it (unwords ["Parses", timeMidnight', "correctly"]) $ do
                toList (parseString timeParser mempty timeMidnight')
                    `shouldBe` [timeMidnight]
            it (unwords ["Parses", timeOther', "correctly"]) $ do
                toList (parseString timeParser mempty timeOther')
                    `shouldBe` [timeOther]
        describe "Parse activities" $ do
            it "Parses an activity correctly" $ do
                toList (parseString activityParser mempty activityTest')
                    `shouldBe` [activityTest]
            it "Parses a commented activity correctly" $ do
                toList (parseString activityParser mempty activityHomeWithComment')
                    `shouldBe` [activityHome]
        describe "Parse dates" $ do
            it "Parses a date correctly" $ do
                toList (parseString dateParser mempty date1')
                    `shouldBe` [date1]
            it "Parses a commented date correctly" $ do
                toList (parseString dateParser mempty date2WithComment')
                    `shouldBe` [date2]
        describe "Parse single log day" $ do
            it "Parses a log day correctly" $ do
                toList (parseString logDayParser mempty logDay1')
                    `shouldBe` [logDay1]
