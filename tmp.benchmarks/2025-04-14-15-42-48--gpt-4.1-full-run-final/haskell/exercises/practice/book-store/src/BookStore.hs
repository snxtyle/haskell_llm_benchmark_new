module BookStore (total, Book(..)) where

import Data.List (sort, group)
import Data.Function (on)

data Book = First | Second | Third | Fourth | Fifth
    deriving (Eq, Ord, Show)

-- Price in cents
bookPrice :: Int
bookPrice = 800

-- Discount table: group size -> discount
discount :: Int -> Double
discount 1 = 0.0
discount 2 = 0.05
discount 3 = 0.10
discount 4 = 0.20
discount 5 = 0.25
discount _ = 0.0

-- Given a basket, return the minimal total price in cents
total :: [Book] -> Int
total books = minimum $ map round $ allGroupings (bookCounts books)

-- Count the number of each book in the basket
bookCounts :: [Book] -> [Int]
bookCounts bs = map (\b -> count b bs) allBooks
  where
    allBooks = [First, Second, Third, Fourth, Fifth]
    count b = length . filter (== b)

-- Generate all possible groupings and calculate their total price
allGroupings :: [Int] -> [Double]
allGroupings counts
    | all (==0) counts = [0]
    | otherwise = [ groupCost n + rest
                  | n <- [1..5]
                  , canTake n counts
                  , let newCounts = removeN n counts
                  , rest <- allGroupings newCounts
                  ]
  where
    groupCost n = fromIntegral n * fromIntegral bookPrice * (1 - discount n)

-- Can we take a group of n different books from the current counts?
canTake :: Int -> [Int] -> Bool
canTake n counts = length (filter (>0) counts) >= n

-- Remove one copy from n different books
removeN :: Int -> [Int] -> [Int]
removeN n counts = zipWith (-) counts (takeN n counts)

-- Take up to n books, one from each nonzero count
takeN :: Int -> [Int] -> [Int]
takeN 0 _ = replicate 5 0
takeN n counts = go n counts
  where
    go 0 cs = replicate 5 0
    go m cs =
        let idxs = [i | (i, c) <- zip [0..] cs, c > 0]
            takeIdxs = take m idxs
        in [if i `elem` takeIdxs then 1 else 0 | i <- [0..4]]

