module BookStore (total, Book(..)) where

data Book = First | Second | Third | Fourth | Fifth
  deriving (Eq, Ord)

total :: [Book] -> Int
total basket = minimum $ map calculatePrice $ allGroupings counts
  where
    counts = bookCounts basket

-- Count occurrences of each book type
bookCounts :: [Book] -> [Int]
bookCounts books = map countBook [First, Second, Third, Fourth, Fifth]
  where
    countBook book = length $ filter (== book) books

-- Generate all possible groupings
allGroupings :: [Int] -> [[[Int]]]
allGroupings counts
  | all (== 0) counts = [[]]
  | otherwise = concatMap (\group -> map (group:) (allGroupings (zipWith (-) counts group))) validGroups
  where
    validGroups = filter (canTakeGroup counts) [[1,1,1,1,1], [1,1,1,1,0], [1,1,1,0,0], [1,1,0,0,0], [1,0,0,0,0]]

-- Check if we can take a group from current counts
canTakeGroup :: [Int] -> [Int] -> Bool
canTakeGroup counts group = all (>= 0) (zipWith (-) counts group)

-- Calculate price for a grouping
calculatePrice :: [[Int]] -> Int
calculatePrice groups = sum $ map groupPrice groups

-- Calculate price for a single group
groupPrice :: [Int] -> Int
groupPrice group = 
  let numBooks = sum group
      basePrice = numBooks * 800
      discount = case numBooks of
        0 -> 0
        1 -> 0
        2 -> 5
        3 -> 10
        4 -> 20
        5 -> 25
        _ -> 0
  in basePrice * (100 - discount) `div` 100
