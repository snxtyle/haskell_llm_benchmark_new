module Change (findFewestCoins) where

import Data.List (sort, minimumBy)
import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as Map

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins
  | target < 0 = Nothing
  | target == 0 = Just []
  | null coins = Nothing
  | target < minimum coins = Nothing
  | otherwise = fmap reverse $ go target
  where
    sortedCoins = sort coins
    -- Memoization to avoid recomputation
    go = memo Map.empty
    memo cache 0 = Just []
    memo cache amt
      | amt < 0 = Nothing
      | otherwise =
          case Map.lookup amt cache of
            Just res -> res
            Nothing ->
              let options = [ fmap (c:) (memo cache (amt - c)) | c <- sortedCoins, c <= amt ]
                  valid = catMaybes options
                  best = if null valid then Nothing else Just (minimumBy (\a b -> compare (length a) (length b)) valid)
                  newCache = Map.insert amt best cache
              in best

