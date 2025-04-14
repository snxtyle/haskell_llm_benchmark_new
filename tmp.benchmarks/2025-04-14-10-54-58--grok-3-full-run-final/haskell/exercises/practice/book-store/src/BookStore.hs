module BookStore (total, Book(..)) where

data Book = First | Second | Third | Fourth | Fifth deriving (Eq, Enum, Bounded)

total :: [Book] -> Int
total basket = minimum $ map calculateCost (possibleGroupings counts)
  where
    -- Count occurrences of each book
    counts = map (\b -> length $ filter (== b) basket) [First .. Fifth]

-- Generate all possible groupings of books to maximize discount
possibleGroupings :: [Int] -> [[Int]]
possibleGroupings counts
  | all (== 0) counts = [[]]
  | otherwise = concatMap (\size -> if canFormGroup size counts then map (size:) (possibleGroupings (removeBooks size counts)) else []) [5,4,3,2,1]
  where
    canFormGroup size cs = length (filter (> 0) cs) >= size
    removeBooks size cs = let decrement = take size (filter (> 0) cs) in
                          zipWith (\c d -> if d > 0 then c - 1 else c) cs (decrement ++ repeat 0)

-- Calculate cost for a specific grouping of books
calculateCost :: [Int] -> Int
calculateCost grouping = sum $ map (\size -> round (fromIntegral (size * 800) * (1.0 - discountRate size))) grouping
  where
    discountRate size = case size of
      5 -> 0.25
      4 -> 0.2
      3 -> 0.1
      2 -> 0.05
      _ -> 0.0
