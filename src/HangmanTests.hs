module HangmanTests where

import Test.Hspec
import Hangman

main :: IO ()
main = hspec $ do
    describe "fillInCharacter test" $ do
        it "no change when guessing existing char" $ do
            let puzz = Puzzle "test" [Just 't', Nothing, Nothing, Just 't'] []
            fillInCharacter puzz 't' `shouldBe` puzz
        it "proper puzzle update on correct guess" $ do
            let puzz = Puzzle "test" [Just 't', Nothing, Nothing, Just 't'] []
            let puzzGuessed = Puzzle "test" [Just 't', Nothing, Just 's', Just 't'] []
            fillInCharacter puzz 's' `shouldBe` puzzGuessed
        it "proper puzzle update on wrong guess" $ do
            let puzz = Puzzle "test" [Just 't', Nothing, Nothing, Just 't'] []
            let puzzGuessed = Puzzle "test" [Just 't', Nothing, Nothing, Just 't'] ['x']
            fillInCharacter puzz 'x' `shouldBe` puzzGuessed

    describe "hangleGuess test" $ do
        it "no change when guessing existing char" $ do
            let puzz = Puzzle "test" [Just 't', Nothing, Nothing, Just 't'] []
            puzzIO <- handleGuess puzz 't'
            puzzIO `shouldBe` puzz
        it "proper puzzle update on correct guess" $ do
            let puzz = Puzzle "test" [Just 't', Nothing, Nothing, Just 't'] []
            puzzIO <- handleGuess puzz 's'
            let puzzGuessed = Puzzle "test" [Just 't', Nothing, Just 's', Just 't'] []
            puzzIO `shouldBe` puzzGuessed
        it "proper puzzle update on wrong guess" $ do
            let puzz = Puzzle "test" [Just 't', Nothing, Nothing, Just 't'] []
            puzzIO <- handleGuess puzz 'x'
            let puzzGuessed = Puzzle "test" [Just 't', Nothing, Nothing, Just 't'] ['x']
            puzzIO `shouldBe` puzzGuessed
