module BookStore (total, Book(..)) where

import Data.List (sort, group)

data Book = First | Second | Third | Fourth | Fifth
  deriving (Eq, Ord)

-- Price of a single book in cents
bookPrice :: Int
bookPrice = 800

-- Calculate discount based on number of different books
discount :: Int -> Double
discount 1 = 0.0
discount 2 = 0.05
discount 3 = 0.10
discount 4 = 0.20
discount 5 = 0.25
discount _ = 0.0

-- Calculate price for a set of different books
setPrice :: Int -> Int
setPrice n = round $ fromIntegral (n * bookPrice) * (1 - discount n)

-- Count occurrences of each book type
countBooks :: [Book] -> [Int]
countBooks books = 
  let bookCounts = [count First, count Second, count Third, count Fourth, count Fifth]
  in bookCounts
  where
    count bookType = length $ filter (== bookType) books

-- Calculate total price finding the optimal grouping
total :: [Book] -> Int
total [] = 0
total books = 
  let counts = countBooks books
      totalBooks = sum counts
  in if totalBooks == 0 then 0 else findOptimalPrice counts

-- Find the optimal price by trying different groupings
findOptimalPrice :: [Int] -> Int
findOptimalPrice counts = minimum $ map calculatePrice allGroupings
  where
    -- Generate all possible groupings
    allGroupings = generateGroupings counts
    
    -- Calculate price for a specific grouping
    calculatePrice :: [Int] -> Int
    calculatePrice grouping = sum $ map setPrice $ filter (> 0) grouping

-- Generate all possible valid groupings for the given book counts
generateGroupings :: [Int] -> [[Int]]
generateGroupings counts = go counts []
  where
    go :: [Int] -> [Int] -> [[Int]]
    go cs groups
      | all (== 0) cs = [reverse groups]
      | otherwise = concatMap tryGroup [5, 4, 3, 2, 1]
      where
        tryGroup :: Int -> [[Int]]
        tryGroup n
          | canTakeGroup n cs = go (takeGroup n cs) (n : groups)
          | otherwise = []
    
    -- Check if we can take a group of n different books
    canTakeGroup :: Int -> [Int] -> Bool
    canTakeGroup n cs = length (filter (> 0) cs) >= n
    
    -- Take a group of n different books from counts
    takeGroup :: Int -> [Int] -> [Int]
    takeGroup n cs = 
      let sorted = reverse $ sort $ zip cs [0..4]
          (toTake, rest) = splitAt n sorted
          taken = map (\(c, i) -> (c - 1, i)) toTake
          allItems = taken ++ rest
          result = map fst $ sort $ map (\(c, i) -> (i, c)) allItems
      in map snd $ sort result

-- Alternative optimized approach using dynamic programming concepts
-- We know that groups of 4 are often better than groups of 5+3
findOptimalPrice :: [Int] -> Int
findOptimalPrice counts = calculateBestPrice counts
  where
    calculateBestPrice :: [Int] -> Int
    calculateBestPrice cs
      | all (== 0) cs = 0
      | otherwise = 
          let possibleSizes = filter (\n -> canMakeGroup n cs) [1..5]
              prices = map (\n -> setPrice n + calculateBestPrice (removeGroup n cs)) possibleSizes
          in if null prices then 0 else minimum prices
    
    -- Check if we can make a group of size n
    canMakeGroup :: Int -> [Int] -> Bool
    canMakeGroup n cs = length (filter (> 0) cs) >= n
    
    -- Remove a group of size n from counts
    removeGroup :: Int -> [Int] -> [Int]
    removeGroup n cs = 
      let indexed = zip cs [0..4]
          sorted = reverse $ sort indexed
          (toRemove, keep) = splitAt n sorted
          removed = map (\(c, i) -> (c - 1, i)) toRemove
          allPairs = removed ++ keep
          result = map snd $ sort $ map (\(c, i) -> (i, c)) allPairs
      in result
