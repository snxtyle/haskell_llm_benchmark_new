module BookStore (total, Book(..)) where

import Data.List (sort, group, nub, delete)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Function (fix) -- For fixed-point recursion with memoization

-- Define the Book data type as specified
data Book = First | Second | Third | Fourth | Fifth deriving (Eq, Ord, Show, Enum, Bounded)

-- Base price of a single book in cents
bookPrice :: Int
bookPrice = 800

-- Discount factors for sets of different sizes
-- Index 0 is unused, index 1 is for 1 book (no discount), etc.
-- The problem states discounts for 2, 3, 4, 5 different books.
-- A single book has no discount, so its factor is 1.0.
discountFactors :: [Double]
discountFactors = [0.0, 1.0, 0.95, 0.90, 0.80, 0.75]

-- Calculate the cost of a set of 'n' unique books
costOfSet :: Int -> Int
costOfSet n = floor $ fromIntegral (n * bookPrice) * (discountFactors !! n)

-- Convert a basket of books to a frequency list [Int] for books First to Fifth
-- This ensures a consistent order for memoization keys (e.g., [count_First, count_Second, ...])
basketToCountsList :: [Book] -> [Int]
basketToCountsList basket =
    let
        initialCounts = replicate 5 0 -- Initialize counts for 5 book types to 0
        -- Function to update counts for a given book
        updateCounts counts book =
            let idx = fromEnum book -- Get integer index for the book (0 to 4)
                val = counts !! idx -- Get current count for this book type
            in take idx counts ++ [val + 1] ++ drop (idx + 1) counts -- Increment count
    in
        foldl updateCounts initialCounts basket

-- Helper function to generate combinations of 'k' elements from a list
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations k (x:xs) =
    map (x:) (combinations (k-1) xs) ++ combinations k xs

-- The memoized recursive function to calculate the minimum cost
-- `f` is the recursive call itself, provided by `fix`.
-- `memoTable` is the current memoization table (Map from [Int] counts to Int cost).
-- `counts` is the current state of the basket (list of book counts).
-- Returns (updatedMemoTable, calculatedCost).
calculateMinCost :: (Map [Int] Int -> [Int] -> (Map [Int] Int, Int)) -> Map [Int] Int -> [Int] -> (Map [Int] Int, Int)
calculateMinCost f memoTable counts =
    case Map.lookup counts memoTable of
        Just cost -> (memoTable, cost) -- Return memoized result if already computed
        Nothing ->
            let
                currentTotalBooks = sum counts
                -- Base case: If no books are left, the cost is 0.
                cost = if currentTotalBooks == 0
                       then 0
                       else
                           let
                               -- Option 1: Calculate cost if all remaining books are bought individually (no discount).
                               -- This serves as an initial upper bound for the minimum cost.
                               costIndividual = currentTotalBooks * bookPrice

                               -- Identify indices of books that are currently available (count > 0)
                               availableBookIndices = [i | (count, i) <- zip counts [0..4], count > 0]

                               -- Explore Option 2: Try to form discounted sets.
                               -- We collect all possible costs from forming a set and recursively calculating the rest.
                               -- `foldl` is used to accumulate the updated memo table and the list of costs.
                               (finalMemoAfterExploringSets, costsFromSets) =
                                   foldl (\(currentMemo, accCosts) k ->
                                       let
                                           -- For each set size `k` (from 2 to 5), find all combinations of unique books
                                           -- that can form a set from the `availableBookIndices`.
                                           kCombinations = combinations k availableBookIndices
                                           -- Calculate cost for each combination and recursively find remaining cost.
                                           (memoAfterK, kCosts) = foldl (\(m, cs) chosenIndices ->
                                               let
                                                   setCost = costOfSet k -- Cost of the current discounted set
                                                   -- Create new counts after taking these `k` books
                                                   newCounts = foldl (\acc idx ->
                                                                      let val = acc !! idx
                                                                      in take idx acc ++ [val - 1] ++ drop (idx + 1) acc
                                                                     ) counts chosenIndices
                                                   -- Recursively call `f` (the memoized function) for the remaining books
                                                   (updatedMemo, remainingCost) = f m newCounts
                                               in
                                                   (updatedMemo, (setCost + remainingCost) : cs)
                                               ) (currentMemo, []) kCombinations
                                       in
                                           (memoAfterK, accCosts ++ kCosts)
                                       ) (memoTable, []) [2..5] -- Iterate through set sizes from 2 to 5

                               -- The minimum cost for the current `counts` is the minimum of:
                               -- 1. Buying all remaining books individually (`costIndividual`).
                               -- 2. All costs derived from forming discounted sets (`costsFromSets`).
                               -- `minimum` handles the case where `costsFromSets` might be empty (e.g., only one type of book left).
                               minCostValue = minimum (costIndividual : costsFromSets)
                           in
                               minCostValue
            in
                -- Store the computed result in the memo table and return it along with the updated table.
                (Map.insert counts cost finalMemoAfterExploringSets, cost)

-- Main function to calculate the total cost of a basket
total :: [Book] -> Int
total basket =
    let
        initialCountsList = basketToCountsList basket
        -- Use `fix` to define the recursive function `f` that refers to itself.
        -- `Map.empty` is the initial empty memoization table.
        (finalMemoTable, finalCost) = fix calculateMinCost Map.empty initialCountsList
    in
        finalCost
