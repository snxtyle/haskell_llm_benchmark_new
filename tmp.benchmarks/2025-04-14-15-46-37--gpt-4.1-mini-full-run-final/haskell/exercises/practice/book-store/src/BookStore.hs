module BookStore (total, Book(..)) where

import Data.List (sort, group)

data Book = First | Second | Third | Fourth | Fifth
  deriving (Eq, Ord, Show)

-- Price per book in cents
bookPrice :: Int
bookPrice = 800

-- Discounts by number of distinct books in a group
discounts :: [(Int, Double)]
discounts = [(1, 1.0), (2, 0.95), (3, 0.90), (4, 0.80), (5, 0.75)]

-- Calculate price for a group of distinct books
groupPrice :: Int -> Int
groupPrice n = round $ fromIntegral (n * bookPrice) * discount
  where
    discount = case lookup n discounts of
      Just d -> d
      Nothing -> 1.0

-- Count how many copies of each book in the basket
countBooks :: [Book] -> [Int]
countBooks books = map length . group . sort $ books

-- Given counts of each book, find all possible groupings and choose the minimal total price
total :: [Book] -> Int
total basket = minimum $ map totalPrice (allGroupings counts)
  where
    counts = countBooks basket

-- Generate all possible groupings of the counts into sets of distinct books
-- Each grouping is a list of group sizes (number of distinct books in each group)
allGroupings :: [Int] -> [[Int]]
allGroupings counts
  | all (==0) counts = [[]]
  | otherwise = do
      groupSize <- [maxGroupSize, maxGroupSize-1 .. 1]
      let newCounts = removeGroup counts groupSize
      case newCounts of
        Nothing -> []
        Just nc -> map (groupSize :) (allGroupings nc)
  where
    maxGroupSize = length (filter (>0) counts)

-- Remove one group of distinct books of size n from counts if possible
-- Returns Nothing if not possible
removeGroup :: [Int] -> Int -> Maybe [Int]
removeGroup counts n
  | length available < n = Nothing
  | otherwise = Just $ sort (decrementN available n ++ rest)
  where
    -- Separate counts into positive and zero counts
    (available, rest) = partitionPos counts

-- Partition counts into positive counts and the rest (zeros)
partitionPos :: [Int] -> ([Int], [Int])
partitionPos = foldr f ([], [])
  where
    f x (pos, zero)
      | x > 0 = (x:pos, zero)
      | otherwise = (pos, x:zero)

-- Decrement n counts by 1
decrementN :: [Int] -> Int -> [Int]
decrementN xs n = map (\(i, x) -> if i < n then x - 1 else x) (zip [0..] xs)

-- Calculate total price for a grouping of group sizes
totalPrice :: [Int] -> Int
totalPrice = sum . map groupPrice
