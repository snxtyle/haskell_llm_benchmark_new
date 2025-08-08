module BookStore (total, Book(..)) where

import Data.List (subsequences, sortOn, foldl')
import Data.Ord (Down(..))

data Book = First | Second | Third | Fourth | Fifth

-- Price for a group of k distinct books, in cents.
priceOf :: Int -> Int
priceOf 0 = 0
priceOf 1 = 800
priceOf 2 = 1520
priceOf 3 = 2160
priceOf 4 = 2560
priceOf 5 = 3000
priceOf _ = error "Invalid group size"

-- Convert the basket into a canonical count vector of length 5, sorted descending.
countsFrom :: [Book] -> [Int]
countsFrom = canon . foldl' step [0,0,0,0,0]
  where
    step [a,b,c,d,e] First  = [a+1,b,c,d,e]
    step [a,b,c,d,e] Second = [a,b+1,c,d,e]
    step [a,b,c,d,e] Third  = [a,b,c+1,d,e]
    step [a,b,c,d,e] Fourth = [a,b,c,d+1,e]
    step [a,b,c,d,e] Fifth  = [a,b,c,d,e+1]
    step _ _ = error "Invalid internal state"

canon :: [Int] -> [Int]
canon = sortOn Down

type Key = [Int]
type Memo = [(Key, Int)]

-- Compute minimal cost for the given multiset of counts (canonical key).
minCost :: Memo -> Key -> (Int, Memo)
minCost memo key
  | all (== 0) key = (0, memo)
  | otherwise =
      case lookup key memo of
        Just v  -> (v, memo)
        Nothing ->
          let nzIdxs = [ i | (i,c) <- zip [0..] key, c > 0 ]
              -- Consider all non-empty subsets of indices with positive counts.
              allGroups = filter (not . null) (subsequences nzIdxs)
              (bestCost, memoFinal) = foldl' (tryGroup key) (maxBound :: Int, memo) allGroups
              memoFinal' = (key, bestCost) : memoFinal
          in (bestCost, memoFinal')

-- Try taking one copy from the given subset of indices; update best and memo.
tryGroup :: Key -> (Int, Memo) -> [Int] -> (Int, Memo)
tryGroup key (bestSoFar, memo) subset =
  let k = length subset
      decremented = [ if i `elem` subset then c - 1 else c | (i, c) <- zip [0..] key ]
      nextKey = canon decremented
      (restCost, memo') = minCost memo nextKey
      totalCost = priceOf k + restCost
      best' = min bestSoFar totalCost
  in (best', memo')

total :: [Book] -> Int
total = fst . minCost [] . countsFrom
