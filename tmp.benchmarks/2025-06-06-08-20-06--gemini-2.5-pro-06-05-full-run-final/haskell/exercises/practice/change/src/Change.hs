module Change (findFewestCoins) where

import Data.Array (Array, listArray, (!))
import Data.Function (on)
import Data.List (minimumBy)
import Data.Maybe (mapMaybe)

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins
  | target < 0 = Nothing
  | otherwise = solutions ! fromIntegral target
  where
    -- It's safer to only work with positive coin values.
    positiveCoins = filter (> 0) coins

    -- DP table, using an Array for O(1) lookups.
    -- It's lazily computed thanks to Haskell's evaluation strategy.
    -- The table stores the optimal coin list for each amount from 0 to target.
    solutions :: Array Int (Maybe [Integer])
    solutions = listArray (0, fromIntegral target) (map solve [0 .. target])

    -- solve computes the best coin combination for a given amount `n`.
    solve :: Integer -> Maybe [Integer]
    solve 0 = Just []
    solve n =
      let -- For each coin, we see if it can be part of a solution for amount `n`.
          candidates =
            mapMaybe
              ( \coin ->
                  if n >= coin
                    then -- If we use `coin`, we need the solution for the remainder `n - coin`.
                         -- We look this up in our `solutions` table and prepend the current coin.
                      (coin :) <$> solutions ! fromIntegral (n - coin)
                    else Nothing
              )
              positiveCoins
       in -- From all valid candidate solutions, we choose the one with the fewest coins.
          case candidates of
            [] -> Nothing
            cs -> Just $ minimumBy (compare `on` length) cs
