module Palindrome where

import Control.Monad
import Data.Char (isAlphaNum, toLower)
import System.Exit (exitSuccess)

palindromeIO :: IO ()
palindromeIO = forever $ do
    line1 <- getLine
    case palindrome line1 of
         True -> putStrLn "It's a palindrome!"
         False -> do
            putStrLn "Nope!"
            exitSuccess

palindrome :: String -> Bool
palindrome s = str == reverse str
    where str = map toLower $ filter isAlphaNum s
