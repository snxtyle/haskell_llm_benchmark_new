module BookStore (total, Book(..)) where

import Data.Bits (testBit)

data Book = First | Second | Third | Fourth | Fifth deriving (Eq, Enum)

-- Prices per book in a group of k unique books (in cents, after discount)
-- k=1: 800 cents (no discount)
-- k=2: 760 cents (5% off)
-- k=3: 720 cents (10% off)
-- k=4: 640 cents (20% off)
-- k=5: 600 cents (25% off)
prices :: [Int]
prices = [800, 760, 720, 640, 600]  -- Indexed from 0 for k=1 to 5

total :: [Book] -> Int
total basket = minCost counts
  where
    -- Get counts for each book: [count of First, count of Second, ..., count of Fifth]
    counts = [length (filter (== book) basket) | book <- [First .. Fifth]]

minCost :: [Int] -> Int  -- [Int] represents counts for books 0 to 4
minCost counts
  | all (== 0) counts = 0  -- Base case: no books left
  | otherwise = minimum [costOfGroup + minCost newCounts
                        | mask <- [1 :: Int .. 31],  -- All non-empty subsets (2^5 - 1)
                          let subset = [i | i <- [0..4], testBit mask i],  -- Books in this subset
                          all (\i -> counts !! i > 0) subset,  -- Can form the group?
                          let k = length subset,  -- Number of unique books
                          k >= 1,
                          let costOfGroup = k * (prices !! (k - 1)),  -- Cost for this group
                          let newCounts = [if i `elem` subset then counts !! i - 1 else counts !! i | i <- [0..4]]  -- Updated counts
                        ]
