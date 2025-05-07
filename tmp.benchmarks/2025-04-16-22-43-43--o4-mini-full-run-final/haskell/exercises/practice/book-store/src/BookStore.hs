module BookStore (total, Book(..)) where

import Data.List (sort)

data Book = First | Second | Third | Fourth | Fifth
  deriving (Eq, Ord, Show)

-- | Calculate the total price in cents for a basket of books,
--   applying the best possible discounts.
total :: [Book] -> Int
total basket = sum [ price groupSize | groupSize <- optimizeGroups counts ]
  where
    -- counts[i] = number of copies of the i-th book
    counts :: [Int]
    counts = [ count b basket | b <- [First, Second, Third, Fourth, Fifth] ]

    count :: Book -> [Book] -> Int
    count x = length . filter (== x)

    -- Make greedy groups: at each step take one copy of each available title
    makeGroups :: [Int] -> [Int]
    makeGroups cs
      | all (== 0) cs = []
      | otherwise     = k : makeGroups cs'
      where
        k   = length (filter (> 0) cs)
        cs' = map (\x -> if x > 0 then x - 1 else 0) cs

    -- Replace each pair of a size-5 group and a size-3 group with two size-4 groups
    adjust :: [Int] -> [Int]
    adjust gs = replicate n5' 5
              ++ replicate n4' 4
              ++ replicate n3' 3
              ++ replicate n2 2
              ++ replicate n1 1
      where
        n1  = length $ filter (== 1) gs
        n2  = length $ filter (== 2) gs
        n3  = length $ filter (== 3) gs
        n4  = length $ filter (== 4) gs
        n5  = length $ filter (== 5) gs
        m   = min n5 n3
        n5' = n5 - m
        n3' = n3 - m
        n4' = n4 + 2 * m

    optimizeGroups :: [Int] -> [Int]
    optimizeGroups = adjust . makeGroups

    -- Price for a group of N different books (in cents)
    price :: Int -> Int
    price n = n * 800 * (100 - discount n) `div` 100

    discount :: Int -> Int
    discount 1 = 0
    discount 2 = 5
    discount 3 = 10
    discount 4 = 20
    discount 5 = 25
    discount _ = 0
