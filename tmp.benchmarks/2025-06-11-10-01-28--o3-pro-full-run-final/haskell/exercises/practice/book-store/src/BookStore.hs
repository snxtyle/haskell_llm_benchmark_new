module BookStore (total, Book(..)) where

import           Data.List       (subsequences)
import qualified Data.Map.Strict as Map

-- | The five books of the series.
data Book = First | Second | Third | Fourth | Fifth
          deriving (Eq, Ord, Show)

-- | Public API – compute the cheapest possible price (in cents) for a basket.
total :: [Book] -> Int
total books = fst $ cheapest initialCounts Map.empty
  where
    initialCounts = toCounts books

-- ------------------------------------------------------------------
-- Internal representation helpers

-- A compact, fixed-length representation of the basket.  Using a tuple
-- keeps look-ups in the memo map fast and hash-friendly.
type Counts = (Int, Int, Int, Int, Int)

toCounts :: [Book] -> Counts
toCounts = foldr add (0, 0, 0, 0, 0)
  where
    add First  (a, b, c, d, e) = (a + 1, b    , c    , d    , e    )
    add Second (a, b, c, d, e) = (a    , b + 1, c    , d    , e    )
    add Third  (a, b, c, d, e) = (a    , b    , c + 1, d    , e    )
    add Fourth (a, b, c, d, e) = (a    , b    , c    , d + 1, e    )
    add Fifth  (a, b, c, d, e) = (a    , b    , c    , d    , e + 1)

countsToList :: Counts -> [Int]
countsToList (a, b, c, d, e) = [a, b, c, d, e]

listToCounts :: [Int] -> Counts
listToCounts [a, b, c, d, e] = (a, b, c, d, e)
listToCounts _               = error "listToCounts: invalid input length"

zeroCounts :: Counts -> Bool
zeroCounts (0, 0, 0, 0, 0) = True
zeroCounts _               = False

-- ------------------------------------------------------------------
-- Pricing

bookPrice :: Int
bookPrice = 800    -- cents

discountPercent :: Int -> Int
discountPercent n = case n of
  1 -> 0
  2 -> 5
  3 -> 10
  4 -> 20
  5 -> 25
  _ -> error "discountPercent: invalid group size"

groupPrice :: Int -> Int
groupPrice size =
  size * bookPrice * (100 - discountPercent size) `div` 100

-- ------------------------------------------------------------------
-- Search for the cheapest grouping (with memoisation)

-- | Returns (cheapestPrice, updatedMemo).
cheapest :: Counts -> Map.Map Counts Int -> (Int, Map.Map Counts Int)
cheapest counts memo
  | zeroCounts counts = (0, memo)
  | Just p <- Map.lookup counts memo = (p, memo)
  | otherwise = (bestPrice, Map.insert counts bestPrice memo')
  where
    freqs            = countsToList counts
    availableIndices = [ i | (i, c) <- zip [0 ..] freqs, c > 0]

    -- Generate every non-empty subset of available indices.
    possibleGroups = filter (not . null) (subsequences availableIndices)

    evaluate :: [[Int]] -> Int -> Map.Map Counts Int -> (Int, Map.Map Counts Int)
    evaluate [] currentBest m = (currentBest, m)
    evaluate (g:gs) currentBest m =
      let nextCounts          = applyGroup g counts
          groupCost           = groupPrice (length g)
          (restCost, m')      = cheapest nextCounts m
          totalCost           = groupCost + restCost
          newBest             = min currentBest totalCost
      in evaluate gs newBest m'

    (bestPrice, memo') = evaluate possibleGroups maxBound memo

-- Apply a chosen group (list of indices) to a count tuple, removing one from
-- each selected slot.
applyGroup :: [Int] -> Counts -> Counts
applyGroup indices = listToCounts . zipWith decrement [0 ..] . countsToList
  where
    decrement i c
      | i `elem` indices = c - 1
      | otherwise        = c

-- ------------------------------------------------------------------
-- End of module
