isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x _  Nothing = x
mayybee _ f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe x = mayybee x id

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList  Nothing = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr f []
    where f  Nothing acc = acc
          f (Just v) acc = v : acc

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs = if or $ map isNothing xs
               then Nothing
               else Just $ catMaybes xs
