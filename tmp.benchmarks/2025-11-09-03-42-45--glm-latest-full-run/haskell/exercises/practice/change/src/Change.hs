module Change (findFewestCoins) where

import Data.List (minimumBy, sort)
import Data.Function (on)
import Data.Maybe (isJust, fromJust)

-- | Find the fewest number of coins needed to make a given amount of change.
--   If it's impossible to make the change, return Nothing.
--
--   This function uses dynamic programming to find the optimal solution.
--   It builds a table of solutions for all amounts from 0 up to the target.
--   For each amount, it finds the best solution by trying every coin and
--   looking up the optimal solution for the remaining amount.
--
--   This approach is guaranteed to find the optimal solution for any set
--   of coins, unlike a greedy algorithm which can fail for non-standard
--   coin systems (e.g., coins = [1, 3, 4], target = 6).
findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins
    -- Edge case: negative change is not possible.
    | target < 0 = Nothing
    -- Edge case: zero change requires zero coins.
    | target == 0 = Just []
    -- Edge case: if no coins are provided, no change can be made.
    | null coins = Nothing
    | otherwise = dp !! fromIntegral target
  where
    -- Sort coins in descending order. This is a small optimization that
    -- can help find shorter solutions earlier, but it doesn't affect
    -- the correctness of the final result.
    sortedCoins = reverse $ sort coins

    -- The DP table. dp !! i contains the optimal list of coins for amount i.
    -- This is an example of "tying the knot" in Haskell, where the list
    -- refers to itself. The runtime system will compute each element only
    -- once as needed.
    dp :: [Maybe [Integer]]
    dp = map findOptimal [0..target]

    -- Helper function to find the optimal solution for a specific amount 'i'.
    findOptimal :: Integer -> Maybe [Integer]
    findOptimal 0 = Just []
    findOptimal i =
        let
            -- Generate all possible solutions for amount 'i' by prepending a coin
            -- to a known solution for the remaining amount (i - coin).
            possibleSolutions = [ coin : fromJust subSolution
                                | coin <- sortedCoins
                                , coin <= i
                                , let subSolution = dp !! fromIntegral (i - coin)
                                , isJust subSolution
                                ]
        in
            -- If no solutions were found, it's impossible to make this amount.
            if null possibleSolutions
            then Nothing
            -- Otherwise, find the solution with the minimum number of coins.
            else Just $ minimumBy (compare `on` length) possibleSolutions
