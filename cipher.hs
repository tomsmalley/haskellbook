module Cipher where

import Data.Char
import Text.Read (readMaybe)

rot :: Int -> Char -> Char
rot shift c
    | isLower c = chr . (+) 97 $ mod (relOrd c + shift) 26
    | isUpper c = chr . (+) 65 $ mod (relOrd c + shift) 26
    | otherwise = c

relOrd :: Char -> Int
relOrd c
    | isLower c = (ord c - ord 'a')
    | isUpper c = (ord c - ord 'A')
    | otherwise = 0

caesar :: Int -> String -> String
caesar shift = map (rot shift)

unCaesar :: Int -> String -> String
unCaesar = caesar . negate

caesarIO :: IO String
caesarIO = do
    putStr "Rotate by: "
    i <- getLine
    case (readMaybe i) :: Maybe Int of
         Nothing -> return "Must be an int!"
         Just x -> do
            putStr "Message: "
            str <- getLine
            return $ "Cipher: " ++ caesar x str

vignere :: String -> String -> String
vignere _ [] = []
vignere [] msg = msg
vignere key@(k:ks) (m:ms)
    | isLower m || isUpper m = f m : vignere (ks ++ [k]) ms
    | otherwise = m : vignere key ms
    where f x = rot (relOrd k) x

unVignere :: String -> String -> String
unVignere = vignere . map (\x -> rot (26 - 2 * (relOrd x)) x)

vignereIO :: IO String
vignereIO = do
    putStr "Key: "
    key <- getLine
    putStr "Message: "
    msg <- getLine
    return $ "Cipher: " ++ vignere key msg

vigRec :: String -> String -> [String]
vigRec key msg = msg : vigRec key (vignere key msg)

vigLoop :: String -> String -> [String]
vigLoop key msg = msg : (takeWhile ((/=) msg) . drop 1 $ vigRec key msg)
