module BookStore (total, Book(..)) where

import Data.Bits        (popCount, testBit)
import qualified Data.Map.Strict as Map
import Data.List        (foldl')

-- | The five different books in the series
data Book = First | Second | Third | Fourth | Fifth
  deriving (Eq, Ord, Show)

-- | Public API: given a basket of books, return the minimal price in cents
total :: [Book] -> Int
total basket = fst (minCost Map.empty initialCounts)
  where
    initialCounts = bookCounts basket

-- ------------------------------------------------------------------
-- Internal helpers
-- ------------------------------------------------------------------

-- Represent the basket as a fixed‑size list with the amount of copies
-- we own for each title, always in the order First … Fifth.
type Counts = [Int]  -- Always length 5

-- Calculate how many copies of each book are in the basket.
bookCounts :: [Book] -> Counts
bookCounts = foldl' step [0, 0, 0, 0, 0]
  where
    step :: Counts -> Book -> Counts
    step [f, s, t, fo, fi] book = case book of
      First  -> [f + 1, s,     t,     fo,     fi    ]
      Second -> [f,     s + 1, t,     fo,     fi    ]
      Third  -> [f,     s,     t + 1, fo,     fi    ]
      Fourth -> [f,     s,     t,     fo + 1, fi    ]
      Fifth  -> [f,     s,     t,     fo,     fi + 1]

-- Main recursive function with memoisation.
-- Given a 'Counts' value, it returns the minimum possible cost from
-- this state and an updated memo table.
minCost :: Map.Map Counts Int -> Counts -> (Int, Map.Map Counts Int)
minCost memo counts
  | all (== 0) counts                = (0, memo)         -- nothing left to buy
  | Just c <- Map.lookup counts memo = (c, memo)         -- already computed
  | otherwise                        = (best, memoFinal)
  where
    -- All subsets (as bit masks) that can be taken from the current counts
    validMasks :: [Int]
    validMasks =
      [ mask
      | mask <- [1 .. 31]  -- 1 == 00001, 31 == 11111  → every non‑empty subset
      , all (\i -> not (testBit mask i) || counts !! i > 0) [0 .. 4]
      ]

    -- Fold over all possible groups, keeping track of the best price and
    -- propagating the memo table along the way.
    (best, memo') = foldl'
      (\(currentBest, m) mask ->
          let groupSize        = popCount mask
              newCounts        = applyMask mask counts
              (restCost, m')   = minCost m newCounts
              costHere         = priceForGroup groupSize + restCost
              newBest          = min currentBest costHere
          in  (newBest, m'))
      (maxBound :: Int, memo)
      validMasks

    -- After evaluating all possibilities, store the result for the current
    -- state in the memo table.
    memoFinal = Map.insert counts best memo'

-- Apply a mask, removing one copy of every selected book
applyMask :: Int -> Counts -> Counts
applyMask mask = zipWith (\i c -> if testBit mask i then c - 1 else c) [0 ..] 

-- Price for a group of *distinct* books of the given size, in cents.
priceForGroup :: Int -> Int
priceForGroup n = n * 800 * discount `div` 100
  where
    discount :: Int  -- percentage to pay (after discount)
    discount = case n of
      1 -> 100  -- no discount
      2 -> 95   -- 5%
      3 -> 90   -- 10%
      4 -> 80   -- 20%
      5 -> 75   -- 25%
      _ -> error "Invalid group size"

