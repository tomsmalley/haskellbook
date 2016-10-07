import Data.Char (toLower)

newtype Word' = Word' String
    deriving (Eq, Show)

vowels = "aeiou"
consonants = filter (not . flip elem vowels) ['a'..'z']

mkWord :: String -> Maybe Word'
mkWord s = if count vowels s > count consonants s
              then Nothing
              else Just $ Word' s
    where count string = length . filter (flip elem string . toLower)
