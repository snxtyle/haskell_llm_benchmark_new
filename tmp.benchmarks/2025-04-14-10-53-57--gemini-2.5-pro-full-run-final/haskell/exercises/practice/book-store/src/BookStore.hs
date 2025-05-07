module BookStore (total, Book(..)) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List (sortOn, filter)
import Data.Ord (Down(..))

-- Added Eq and Ord to allow books to be keys in a Map and be sorted.
data Book = First | Second | Third | Fourth | Fifth
  deriving (Eq, Ord, Show, Enum, Bounded)

-- Base price of a single book in cents
bookPrice :: Int
bookPrice = 800

-- Pre-calculated prices for discounted groups of size k (index k) in cents.
-- Index 0: 0 books = 0 cents
-- Index 1: 1 book * 800 * 1.00 = 800 cents
-- Index 2: 2 books * 800 * 0.95 = 1520 cents
-- Index 3: 3 books * 800 * 0.90 = 2160 cents
-- Index 4: 4 books * 800 * 0.80 = 2560 cents
-- Index 5: 5 books * 800 * 0.75 = 3000 cents
discountPrices :: [Int]
discountPrices = [0, 800, 1520, 2160, 2560, 3000]

-- Get the price for a group of k distinct books
priceForGroupSize :: Int -> Int
priceForGroupSize k
  | k >= 0 && k < length discountPrices = discountPrices !! k
  | otherwise = error $ "Invalid group size requested: " ++ show k

-- Counts the occurrences of each book in the basket.
countBooks :: [Book] -> Map Book Int
countBooks = Map.fromListWith (+) . map (, 1)

-- Recursive helper function to calculate the minimum price.
-- It takes a list of the counts of each distinct book type remaining.
calculateMinPrice :: [Int] -> Int
calculateMinPrice counts =
  -- Filter out books with zero count and sort counts descending.
  -- Sorting helps in consistently forming groups from the most frequent books,
  -- although the logic works regardless of order due to exploring all k.
  -- Sorting is more crucial if memoization were added for performance on larger inputs.
  let positiveCounts = sortOn Down $ filter (> 0) counts
  in if null positiveCounts
     then 0 -- Base case: no books left, price is 0.
     else minimum $ do -- Use list monad to explore possibilities and find the minimum.
           -- Iterate through all possible group sizes k for the next group.
           -- k can range from 1 to the number of distinct book types available.
           let numDistinctBooks = length positiveCounts
           k <- [1 .. numDistinctBooks]

           -- Calculate the price for forming a group of size k
           let currentGroupPrice = priceForGroupSize k

           -- Determine the remaining book counts after forming this group.
           -- We take one book from k distinct types (represented by the first k counts).
           let (taken, remaining) = splitAt k positiveCounts
           -- Decrement the counts of the books included in the group.
           let decrementedTaken = map (\c -> c - 1) taken
           -- Combine the decremented counts with the counts of books not in the group.
           let nextCounts = decrementedTaken ++ remaining

           -- Recursively calculate the minimum price for the remaining books.
           let remainingPrice = calculateMinPrice nextCounts

           -- Return the total price for this specific choice of group size k.
           return (currentGroupPrice + remainingPrice)

-- Calculate the lowest possible price for the books in the basket.
total :: [Book] -> Int
total basket
  | null basket = 0
  | otherwise =
      -- 1. Count the number of copies of each distinct book.
      let bookCountsMap = countBooks basket
          -- 2. Get the counts as a list of integers.
          initialCounts = Map.elems bookCountsMap
      -- 3. Calculate the minimum price using the recursive helper function.
      in calculateMinPrice initialCounts
