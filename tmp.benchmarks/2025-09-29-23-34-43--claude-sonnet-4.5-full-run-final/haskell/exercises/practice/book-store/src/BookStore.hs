module BookStore (total, Book(..)) where

import Data.List (sort, group, sortBy)
import Data.Ord (comparing, Down(..))

data Book = First | Second | Third | Fourth | Fifth
  deriving (Eq, Ord, Show)

-- Base price per book in cents
basePrice :: Int
basePrice = 800

-- Discount rates based on number of unique books
discount :: Int -> Double
discount 1 = 0.0
discount 2 = 0.05
discount 3 = 0.10
discount 4 = 0.20
discount 5 = 0.25
discount _ = 0.0

-- Calculate price for a group of n unique books
groupPrice :: Int -> Int
groupPrice n = round (fromIntegral (n * basePrice) * (1.0 - discount n))

-- Count frequency of each book type
countBooks :: [Book] -> [Int]
countBooks books = map length $ group $ sort books

-- Generate all possible groupings and find minimum cost
total :: [Book] -> Int
total [] = 0
total basket = minimum $ map calculateTotal $ generateGroupings counts
  where
    counts = sortBy (comparing Down) $ countBooks basket

-- Generate groupings by trying different strategies
generateGroupings :: [Int] -> [[Int]]
generateGroupings counts = [greedyGrouping counts, optimizedGrouping counts]

-- Greedy approach: always form the largest possible groups first
greedyGrouping :: [Int] -> [Int]
greedyGrouping [] = []
greedyGrouping counts
  | all (== 0) counts = []
  | otherwise = groupSize : greedyGrouping remaining
  where
    groupSize = length $ filter (> 0) counts
    remaining = map (\c -> if c > 0 then c - 1 else 0) counts

-- Optimized approach: prefer groups of 4 over groups of 5+3
-- This handles cases like the example where 4+4 is better than 5+3
optimizedGrouping :: [Int] -> [Int]
optimizedGrouping counts = optimize (greedyGrouping counts)

-- Optimize by converting 5+3 pairs into 4+4 pairs when beneficial
optimize :: [Int] -> [Int]
optimize groups = replaceGroups groups
  where
    replaceGroups :: [Int] -> [Int]
    replaceGroups [] = []
    replaceGroups [x] = [x]
    replaceGroups (5:rest) = 
      case findAndReplace 3 rest of
        Just newRest -> 4 : 4 : replaceGroups newRest
        Nothing -> 5 : replaceGroups rest
    replaceGroups (x:xs) = x : replaceGroups xs
    
    findAndReplace :: Int -> [Int] -> Maybe [Int]
    findAndReplace _ [] = Nothing
    findAndReplace target (x:xs)
      | x == target = Just xs
      | otherwise = fmap (x:) (findAndReplace target xs)

-- Calculate total price for a grouping
calculateTotal :: [Int] -> Int
calculateTotal = sum . map groupPrice
