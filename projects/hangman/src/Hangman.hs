module Hangman where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

newtype WordList = WordList [String]
    deriving (Eq, Show)

allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt"
    return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
    (WordList aw) <- allWords
    return $ WordList (filter gameLength aw)
    where gameLength w =
            let l = length (w :: String)
            in l > minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
    randomIndex <- randomRIO (0, length wl - 1)
    return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char]
    deriving (Eq)

instance Show Puzzle where
    show (Puzzle _ discovered guessed) =
        (intersperse ' ' $ fmap renderPuzzleChar discovered)
        ++ " : " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle w = Puzzle w (map (const Nothing) w) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle w _ _) = flip elem w

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ g) = flip elem g

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing  = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filled s) c =
    if (c `elem` word)
       then Puzzle word newFilled s
       else Puzzle word newFilled (c:s)
    where newFilled = zipWith (zipper c) word filled
          zipper guessed wordChar guessChar = if wordChar == guessed
                                               then Just wordChar
                                               else guessChar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
    case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
        (_, True) -> do
            putStrLn "You already guessed that character, pick something else!"
            return puzzle
        (True, _) -> do
            putStrLn "This character was in the word!"
            return (fillInCharacter puzzle guess)
        (False, _) -> do
            putStrLn "Wrong!"
            return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
    if (length guessed) > 7 then
        do putStrLn "You lose!"
           putStrLn $ "The word was: " ++ wordToGuess
           exitSuccess
    else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle word filledInSoFar _ ) =
    if all isJust filledInSoFar then
        do putStrLn $ "You win! The word was: " ++ word
           exitSuccess
    else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
         [c] -> handleGuess puzzle c >>= runGame
         _   -> putStrLn "One character at a time!"
