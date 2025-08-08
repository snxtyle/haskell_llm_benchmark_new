module BookStore (total, Book(..)) where

import Data.List (foldl')

-- The five books in the series
data Book = First | Second | Third | Fourth | Fifth
  deriving (Eq, Ord, Enum, Bounded)

-- Public API: total cost in cents for the given basket
total :: [Book] -> Int
total bs =
  let counts = countsFrom bs
      groups = buildGroups counts
  in priceFromGroups groups

-- Build the count vector (one slot per title)
countsFrom :: [Book] -> [Int]
countsFrom = foldl' (\acc b -> incAt (fromEnum b) acc) (replicate bookKinds 0)
  where
    bookKinds = length allBooks

-- Increment a position in a list by one (0-based index)
incAt :: Int -> [Int] -> [Int]
incAt _ [] = []
incAt 0 (x:xs) = (x + 1) : xs
incAt n (x:xs) = x : incAt (n - 1) xs

-- All books in order, used for bounds and sizing
allBooks :: [Book]
allBooks = [minBound .. maxBound]

-- Greedily form the largest possible set of distinct books each step.
-- Returns the list of group sizes (each between 1 and 5).
buildGroups :: [Int] -> [Int]
buildGroups counts
  | all (== 0) counts = []
  | otherwise =
      let size = length (filter (> 0) counts)
          counts' = map (\c -> if c > 0 then c - 1 else 0) counts
      in size : buildGroups counts'

-- Compute minimal price (in cents) from the list of group sizes,
-- applying the known optimization: replace as many (5,3) pairs as
-- possible with (4,4), which is cheaper.
priceFromGroups :: [Int] -> Int
priceFromGroups sizes =
  let n1 = countEq 1 sizes
      n2 = countEq 2 sizes
      n3 = countEq 3 sizes
      n4 = countEq 4 sizes
      n5 = countEq 5 sizes
      pair53 = min n5 n3
      -- Prices per group size in cents
      p1 = 800
      p2 = 1520
      p3 = 2160
      p4 = 2560
      p5 = 3000
  in  n1 * p1
    + n2 * p2
    + (n3 - pair53) * p3
    + (n5 - pair53) * p5
    + (n4 + 2 * pair53) * p4

countEq :: Int -> [Int] -> Int
countEq k = length . filter (== k)
