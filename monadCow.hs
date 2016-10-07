justIf :: (a -> Bool) -> a -> Maybe a
justIf p x = if p x then Just x else Nothing

data Cow = Cow { name :: String
               , age :: Int
               , weight :: Int
               } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty = justIf (not . null)

noNegative :: Int -> Maybe Int
noNegative = justIf (>0)

weightCheck :: Cow -> Maybe Cow
weightCheck c = let w = weight c
                    n = name c
                in if n == "Bess" && w > 499
                      then Nothing
                      else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow n a w = Cow
                    <$> noEmpty n
                    <*> noNegative a
                    <*> noNegative w
                    >>= weightCheck
