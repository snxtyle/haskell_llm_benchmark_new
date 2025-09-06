module BookStore (total, Book(..)) where

import Data.List (sortBy)
import Data.Ord (comparing, Down)

data Book = First | Second | Third | Fourth | Fifth deriving (Eq)

price :: Int -> Int
price 1 = 800
price 2 = 1520
price 3 = 2160
price 4 = 2560
price 5 = 3000
price _ = error "Invalid group size"

countBooks :: [Book] -> [Int]
countBooks books = [length $ filter (== b) books | b <- [First, Second, Third, Fourth, Fifth]]

sortDesc :: [Int] -> [Int]
sortDesc = sortBy (flip compare)

canTake :: Int -> [Int] -> Bool
canTake k state = length (filter (>0) state) >= k

subtractK :: Int -> [Int] -> [Int]
subtractK k state = sortDesc $ zipWith (\c i -> if i < k then c - 1 else c) state [0..]

go :: [Int] -> Int
go state
  | all (==0) state = 0
  | otherwise = let possibleKs = filter (\k -> canTake k state) [5,4,3,2]
                in if null possibleKs
                     then sum $ map (*800) state
                     else minimum $ map (\k -> price k + go (subtractK k state)) possibleKs

total :: [Book] -> Int
total basket = go (sortDesc (countBooks basket))
