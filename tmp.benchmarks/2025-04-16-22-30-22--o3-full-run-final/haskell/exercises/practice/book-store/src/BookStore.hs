module BookStore (total, Book(..)) where

import           Control.Monad.State.Strict (State, evalState, get, modify)
import qualified Data.Map.Strict            as M
import           Data.List                  (subsequences)

-- | Each book in the series.
data Book = First | Second | Third | Fourth | Fifth
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | Price (in cents) of a single book.
singlePrice :: Int
singlePrice = 800

-- | For every possible size of a set of distinct books (1‥5) give the price
--   after the corresponding discount, expressed in cents.
--
--   Index 0 corresponds to a set of size 1, index 4 to a set of size 5.
priceTable :: [Int]
priceTable =
  [ 1 * singlePrice                           -- no discount
  , 2 * singlePrice * 95 `div` 100            -- 5 % discount
  , 3 * singlePrice * 90 `div` 100            -- 10 % discount
  , 4 * singlePrice * 80 `div` 100            -- 20 % discount
  , 5 * singlePrice * 75 `div` 100            -- 25 % discount
  ]

-- | Look‑up helper for the price of a set of @n@ distinct books.
priceOfSet :: Int -> Int
priceOfSet n
  | n >= 1 && n <= 5 = priceTable !! (n - 1)
  | otherwise        = error "Invalid set size"

-- | Count how many copies of each title are present in a basket.
--   The result is a list of five non‑negative integers, where the i‑th element
--   is the number of copies of the i‑th 'Book' (using the 'Enum' order).
bookCounts :: [Book] -> [Int]
bookCounts basket = [ countOccurrences b | b <- enumAll ]
  where
    enumAll = [minBound .. maxBound]
    countOccurrences book = length (filter (== book) basket)

-- | Subtract one copy for every index contained in @subset@.
removeSubset :: [Int]   -- ^ Current counts
             -> [Int]   -- ^ Indices (0‥4) to remove one copy from
             -> [Int]
removeSubset counts subsetIndices =
  [ c - if i `elem` subsetIndices then 1 else 0
  | (i, c) <- zip [0 ..] counts
  ]

-- | All non‑empty subsets of indices for which at least one copy is available.
availableSubsets :: [Int] -> [[Int]]
availableSubsets counts =
  filter (not . null) . subsequences $
    [ idx | (idx, c) <- zip [0 ..] counts, c > 0 ]

type Memo = M.Map [Int] Int

-- | Recursive minimum‑cost search with memoisation.
costM :: [Int] -> State Memo Int
costM counts = do
  memo <- get
  case M.lookup counts memo of
    Just v  -> return v
    Nothing -> do
      let result
            | all (== 0) counts = 0
            | otherwise         = minimum
                [ priceOfSet (length subset) + restCost
                | subset <- availableSubsets counts
                , let remaining = removeSubset counts subset
                , restCost <- (:[]) =<< costM remaining
                ]
      modify (M.insert counts result)
      return result

-- | Compute the minimal price (in cents) for a given basket.
total :: [Book] -> Int
total basket = evalState (costM (bookCounts basket)) M.empty
