module ReplaceExperiment where

replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe String]
lms = [Just "Ave", Nothing, Just "woohoo"]

replaceWithP' :: [Maybe String] -> Char
replaceWithP' = replaceWithP

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe String] -> [Char]
liftedReplace' = liftedReplace
