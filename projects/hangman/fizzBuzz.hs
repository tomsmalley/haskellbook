import Control.Monad
import Control.Monad.Trans.State

import qualified Data.DList as DL

dict :: [(Integer, String)]
dict = [ (5, "Fizz")
       , (3, "Buzz")
       , (2, "Whizz")
       ]

fizzBuzz :: Integer -> String
fizzBuzz n = case go dict of
                  "" -> show n
                  x -> x
    where go [] = ""
          go ((i,s):ds) = if n `mod` i == 0 then s ++ go ds else go ds

fizzBuzzF :: Integer -> String
fizzBuzzF n = concat . fromEmpty (show n) . map snd $ filter pred dict
  where pred (i,_) = n `mod` i == 0

fromEmpty :: a -> [a] -> [a]
fromEmpty x [] = [x]
fromEmpty _ xs = xs

fizzBuzzM :: Integer -> String
fizzBuzzM n = foldMap f dict
  where f (i,s) = if n `mod` i == 0 then s else ""

fizzbuzzList :: [Integer] -> DL.DList String
fizzbuzzList list = execState (mapM_ addResult list) DL.empty

addResult :: Integer -> State (DL.DList String) ()
addResult n = do
    xs <- get
    let result = fizzBuzzF n
    put (DL.snoc xs result)

fizzbuzzList' :: [Integer] -> [String]
fizzbuzzList' list = execState (mapM_ addResult' list) []

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo l u = fizzbuzzList' $ [u, (u-1) .. l]

addResult' :: Integer -> State [String] ()
addResult' n = do
    xs <- get
    let result = fizzBuzzF n
    put (result : xs)

main :: IO ()
main = mapM_ putStrLn $ fizzbuzzFromTo 1 100
