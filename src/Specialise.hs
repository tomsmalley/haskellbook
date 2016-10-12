module Specialise where

pureList :: a -> [a]
pureList = pure
appList :: [a -> b] -> [a] -> [b]
appList = (<*>)

pureIO :: a -> IO a
pureIO = pure
appIO :: IO (a -> b) -> IO a -> IO b
appIO = (<*>)

pureTup :: Monoid a => a -> (a,a)
pureTup = pure
appTup :: Monoid a => (a, (a -> b)) -> (a, a) -> (a, b)
appTup = (<*>)

pureFun :: a -> (e -> a)
pureFun = pure
appFun :: (e -> (a -> b)) -> (e -> a) -> (e -> b)
appFun = (<*>)
