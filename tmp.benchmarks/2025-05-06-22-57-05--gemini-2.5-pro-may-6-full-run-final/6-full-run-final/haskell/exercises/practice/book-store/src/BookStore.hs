module BookStore (total, Book(..)) where

import Data.List (foldl')
import qualified Data.Map.Strict as Map

-- Added deriving for Eq (equality), Ord (ordering for Map keys),
-- Show (for debugging), Enum/Bounded (for completeness, can be useful).
data Book = First | Second | Third | Fourth | Fifth
  deriving (Eq, Ord, Show, Enum, Bounded)

bookPrice :: Int
bookPrice = 800 -- Price of a single book in cents

-- Calculates the price for a group of `numBooks` distinct books, applying the discount.
-- Uses integer arithmetic to maintain precision with cents.
priceForGroupSizeInt :: Int -> Int
priceForGroupSizeInt numBooks =
  let baseCost = numBooks * bookPrice -- Cost without discount
  in case numBooks of
    0 -> 0 -- An empty group costs nothing
    1 -> baseCost -- 0% discount: baseCost * 100 `div` 100
    2 -> (baseCost * 95) `div` 100 -- 5% discount
    3 -> (baseCost * 90) `div` 100 -- 10% discount
    4 -> (baseCost * 80) `div` 100 -- 20% discount
    5 -> (baseCost * 75) `div` 100 -- 25% discount
    _ -> error "Invalid group size for discount calculation" -- Should not happen with current logic

-- Helper function to count occurrences of an element in a list.
countElem :: Eq a => a -> [a] -> Int
countElem x = length . filter (==x)

-- Helper function to remove the first `nToRemove` occurrences of `x` from list `xs`.
removeN :: Eq a => Int -> a -> [a] -> [a]
removeN 0 _ xs = xs -- If n is 0, list is unchanged
removeN _ _ [] = [] -- If list is empty, nothing to remove
removeN nToRemove x (y:ys)
  | x == y    = removeN (nToRemove - 1) x ys -- Found one, decrement n, continue with tail
  | otherwise = y : removeN nToRemove x ys   -- Not found, keep y, continue with tail and same n

total :: [Book] -> Int
total basket =
  if null basket
  then 0
  else
    let
      -- 1. Count occurrences of each book type
      initialCounts :: Map.Map Book Int
      initialCounts = foldl' (\acc book -> Map.insertWith (+) book 1 acc) Map.empty basket

      -- 2. Greedily form groups of distinct books
      --    `currentCounts`: remaining books to be grouped
      --    `accGroupSizes`: list of sizes of groups formed so far
      formGroups :: Map.Map Book Int -> [Int] -> [Int]
      formGroups currentCounts accGroupSizes =
        -- Find all distinct book types that are still present in the basket
        let distinctBookTypesPresent = Map.keys (Map.filter (> 0) currentCounts)
            numDistinctTypes = length distinctBookTypesPresent
        in if numDistinctTypes == 0
           then accGroupSizes -- Base case: no more books to group
           else
             -- Form a new group with one of each distinct type available
             -- Decrement count for each book type included in this new group
             let updatedCounts = foldl' (\acc bookType -> Map.adjust (\c -> c - 1) bookType acc) currentCounts distinctBookTypesPresent
             -- Add the size of the newly formed group and recurse
             in formGroups updatedCounts (numDistinctTypes : accGroupSizes)

      -- Groups are added to the head of `accGroupSizes`, so the list is in reverse order of formation.
      -- e.g., for a basket leading to groups of 5 then 3, rawGroupSizes will be [3,5].
      rawGroupSizes = formGroups initialCounts []
      -- Reverse to get the order of formation, e.g., [5,3]. This is mostly for conceptual clarity;
      -- the counts of 5s and 3s are what matter for the swap.
      initialGroupSizes = reverse rawGroupSizes

      -- 3. Apply the (5,3) -> (4,4) optimization
      --    This is because 2 groups of 4 are cheaper than 1 group of 5 + 1 group of 3.
      numFives = countElem 5 initialGroupSizes
      numThrees = countElem 3 initialGroupSizes
      numSwaps = min numFives numThrees -- Number of (5,3) pairs we can swap to (4,4) pairs

      -- Remove the swapped 5s and 3s from the list
      groupsWithoutFives = removeN numSwaps 5 initialGroupSizes
      groupsWithoutFivesAndThrees = removeN numSwaps 3 groupsWithoutFives
      
      -- Add the new 4s resulting from the swaps
      finalGroupSizes = replicate (numSwaps * 2) 4 ++ groupsWithoutFivesAndThrees

      -- 4. Calculate the total price from the final set of group sizes
      finalPrice = sum (map priceForGroupSizeInt finalGroupSizes)
    in
      finalPrice
