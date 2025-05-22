module BookStore (total, Book(..)) where

import Data.List (group, sort)

data Book = First | Second | Third | Fourth | Fifth
  deriving (Eq, Ord)

-- Calculate the total price in cents
total :: [Book] -> Int
total basket = 
  let counts = bookCounts basket
      groups = optimalGrouping counts
  in sum (map groupPrice groups)

-- Count how many of each book we have
bookCounts :: [Book] -> [Int]
bookCounts books = 
  let grouped = group (sort books)
      allBooks = [First, Second, Third, Fourth, Fifth]
      countBook b = length (filter (== b) books)
  in map countBook allBooks

-- Calculate the price for a group of different books
groupPrice :: Int -> Int
groupPrice n = case n of
  1 -> 800
  2 -> 2 * 800 * 95 `div` 100   -- 5% discount
  3 -> 3 * 800 * 90 `div` 100   -- 10% discount
  4 -> 4 * 800 * 80 `div` 100   -- 20% discount
  5 -> 5 * 800 * 75 `div` 100   -- 25% discount
  _ -> 0

-- Find the optimal grouping of books
optimalGrouping :: [Int] -> [Int]
optimalGrouping counts = 
  let groups = extractGroups counts
      -- Special case: if we have groups of 5 and 3, try converting to two groups of 4
      optimized = optimizeGroups groups
  in optimized

-- Extract groups greedily, starting with the largest possible groups
extractGroups :: [Int] -> [Int]
extractGroups counts = go counts []
  where
    go cs groups
      | all (== 0) cs = groups
      | otherwise = 
          let nonZero = length (filter (> 0) cs)
              groupSize = min 5 nonZero
              newCounts = map (\c -> if c > 0 then c - 1 else 0) cs
          in go newCounts (groupSize : groups)

-- Optimize groups by converting (5,3) pairs to (4,4) pairs
optimizeGroups :: [Int] -> [Int]
optimizeGroups groups = 
  let count5 = length (filter (== 5) groups)
      count3 = length (filter (== 3) groups)
      pairs = min count5 count3
      -- Remove pairs of (5,3) and add pairs of (4,4)
      removed5 = filter (/= 5) groups ++ replicate (count5 - pairs) 5
      removed3 = filter (/= 3) removed5 ++ replicate (count3 - pairs) 3
      added4 = removed3 ++ replicate (pairs * 2) 4
  in added4
