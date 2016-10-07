module LearnParsers where

import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one = char '1' >> eof >> stop

one' = one >> stop

oneTwo = char '1' >> char '2' >> eof >> stop

oneTwo' = oneTwo >> stop

oneC :: Parser Char
oneC = char '1' >> char '2' >> char '3'
oneS :: Parser String
oneS = string "123"
stringChar (x:xs) = char x >> stringChar xs
stringChar [] = stop

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

strParse :: String -> IO ()
strParse s = print $ parseString oneS mempty s

pNL s = putStrLn ('\n' : s)

main = do
    pNL "1"
    strParse "1"
    pNL "12"
    strParse "12"
    pNL "123"
    strParse "123"
