module BookStore (total, Book(..)) where

-- The five books in the series.
-- deriving Eq, Ord for sorting and grouping, Enum for easy indexing.
data Book = First | Second | Third | Fourth | Fifth
  deriving (Eq, Ord, Show, Enum)

-- Calculates the total price for a basket of books.
total :: [Book] -> Int
total basket
  | null basket = 0
  | otherwise =
      let
        -- 1. Count the number of copies for each of the 5 book types.
        counts = countsFromBasket basket
        -- 2. Greedily form groups of distinct books. This is a good starting
        --    point but not always optimal.
        greedyGroups = groupBooks counts
        -- 3. Apply an optimization: a pair of (5-book, 3-book) groups is more
        --    expensive than two 4-book groups. We swap them.
        optimizedGroups = optimizePairsOf5And3 greedyGroups
        -- 4. Calculate the final price from the optimized group sizes.
        totalPrice = sum $ map priceForGroupSize optimizedGroups
      in
        totalPrice

-- A list of all possible books, used to ensure our counts list is always size 5.
allBookTypes :: [Book]
allBookTypes = [First, Second, Third, Fourth, Fifth]

-- Creates a list of counts for each book type.
-- e.g., [First, First, Second] -> [2, 1, 0, 0, 0]
countsFromBasket :: [Book] -> [Int]
countsFromBasket basket = map (\bookType -> length $ filter (== bookType) basket) allBookTypes

-- Recursively forms groups of distinct books from the counts.
-- On each step, it forms the largest possible group.
groupBooks :: [Int] -> [Int]
groupBooks counts
  | all (== 0) counts = []
  | otherwise =
      let
        -- The size of the group is the number of distinct books we have.
        groupSize = length $ filter (> 0) counts
        -- Decrement the count for each book type included in the group.
        newCounts = map (\c -> if c > 0 then c - 1 else 0) counts
      in
        groupSize : groupBooks newCounts

-- Replaces pairs of (5-book, 3-book) groups with two 4-book groups,
-- as this is cheaper.
optimizePairsOf5And3 :: [Int] -> [Int]
optimizePairsOf5And3 groups =
  let
    numFives = length $ filter (== 5) groups
    numThrees = length $ filter (== 3) groups
    -- Number of pairs we can swap.
    swaps = min numFives numThrees
    -- Groups that are not part of the swap.
    remainingGroups = filter (\g -> g /= 5 && g /= 3) groups
  in
    replicate (numFives - swaps) 5 ++
    replicate (numThrees - swaps) 3 ++
    replicate (swaps * 2) 4 ++
    remainingGroups

-- Price in cents for a discounted group of a given size.
priceForGroupSize :: Int -> Int
priceForGroupSize 1 = 800  -- 0% discount
priceForGroupSize 2 = 1520 -- 5% discount
priceForGroupSize 3 = 2160 -- 10% discount
priceForGroupSize 4 = 2560 -- 20% discount
priceForGroupSize 5 = 3000 -- 25% discount
priceForGroupSize _ = 0
