module ThyFearfulSymmetry where

myWords :: String -> [String]
myWords "" = []
myWords s = (takeWhile (pred) s) : (myWords $ drop 1 $ dropWhile (pred) s)
    where pred x = x /= ' '
