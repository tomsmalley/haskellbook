import Data.Char

cap :: String -> String
cap = map toUpper

rev :: String -> String
rev = reverse

composed :: String -> String
composed = cap . rev

fmapped :: String -> String
fmapped = fmap cap rev

tupled :: String -> (String, String)
--tupled x = (cap x, rev x)
--tupled = (,) <$> cap <*> rev
--tupled = do
--    c <- cap
--    r <- rev
--    return (c, r)
tupled = fmap rev . (cap >>= (,))
