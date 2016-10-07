module WordNumberTest where

import Test.Hspec
import WordNumber (digitToWord, digits, wordNumber)

main :: IO ()
main = hspec $ do
    describe "digitToWord works" $ do
        it "returns zero for 0" $ do
            digitToWord 0 `shouldBe` "zero"
        it "returns one for 1" $ do
            digitToWord 1 `shouldBe` "one"
        it "returns minus-five for -5" $ do
            digitToWord (-5) `shouldBe` "minus-five"

    describe "digits does what we want" $ do
        it "returns [1] for 1" $ do
            digits 1 `shouldBe` [1]
        it "returns [1,0,0] for 100" $ do
            digits 100 `shouldBe` [1,0,0]
        it "returns [-4,5] for -45" $ do
            digits (-45) `shouldBe` [-4, 5]

    describe "wordNumber does what we want" $ do
        it "returns one-zero-zero for 100" $ do
            wordNumber 100 `shouldBe` "one-zero-zero"
        it "returns nine-zero-zero-one for 9001" $ do
            wordNumber 9001 `shouldBe` "nine-zero-zero-one"
        it "returns minus-three-zero for -30" $ do
            wordNumber (-30) `shouldBe` "minus-three-zero"
