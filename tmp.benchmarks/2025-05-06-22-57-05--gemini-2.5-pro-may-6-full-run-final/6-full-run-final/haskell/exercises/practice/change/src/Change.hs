module Change (findFewestCoins) where

import Data.List (sort)
import qualified Data.Map.Strict as Map

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins
  | target < 0 = Nothing -- Negative target is not possible.
  | target == 0 = Just []  -- Target of 0 means 0 coins.
  | otherwise =
      -- Filter out non-positive coins and sort them.
      -- Sorting coins (e.g., ascending) ensures that when multiple coin combinations
      -- yield the same minimum number of coins, the behavior is predictable,
      -- though the specific choice among same-length solutions doesn't affect "fewest".
      -- It also allows an early exit if no valid coins are available.
      let validCoins = sort $ filter (> 0) coins
      in
        -- If there are no valid (positive) coins and the target is positive,
        -- no solution is possible. This check is an optimization; the main logic
        -- would also correctly yield Nothing.
        if null validCoins then
          Nothing
        else
          -- dpMap stores: amount -> Maybe (count_of_coins, list_of_coins_for_amount)
          -- Initialize dpMap with 0 -> Just (0, []), representing 0 coins for amount 0.
          let initialDpMap = Map.singleton 0 (Just (0, []))

              -- Build the DP table iteratively.
              -- 'currentSolutionsMap' accumulates solutions for amounts 0 up to 'currentAmount - 1'.
              -- 'currentAmount' is the amount for which we are computing the solution.
              finalDpMap = foldl (computeSolutionForNextAmount validCoins) initialDpMap [1..target]

          in case Map.lookup target finalDpMap of
               -- If a solution was found, extract the list of coins and sort it.
               -- Sorting the final list of coins is for a canonical/predictable output format.
               Just (Just (_, resultCoins)) -> Just (sort resultCoins)
               _ -> Nothing -- No solution found for the target amount.

-- This function is passed to foldl to compute the solution for 'currentAmountToSolve'.
-- It uses 'dpMapAtPrevStep' which contains solutions for amounts less than 'currentAmountToSolve'.
computeSolutionForNextAmount :: [Integer]         -- List of available valid coins, sorted.
                             -> Map.Map Integer (Maybe (Int, [Integer])) -- DP map with solutions up to 'currentAmountToSolve - 1'.
                             -> Integer           -- The current amount for which to find the fewest coins.
                             -> Map.Map Integer (Maybe (Int, [Integer])) -- Updated DP map including solution for 'currentAmountToSolve'.
computeSolutionForNextAmount validCoins dpMapAtPrevStep currentAmountToSolve =
  let
      -- Iterate through each 'coinToTry' from 'validCoins' to find the best way to make 'currentAmountToSolve'.
      -- 'accBestSolutionForCurrentAmount' holds the best solution found so far for 'currentAmountToSolve'.
      bestSolutionForCurrentAmount =
        foldl (evaluateCoinChoice dpMapAtPrevStep currentAmountToSolve) Nothing validCoins

  in Map.insert currentAmountToSolve bestSolutionForCurrentAmount dpMapAtPrevStep

-- This function evaluates using a 'coinToTry' to make 'currentAmountToSolve'.
-- It's used as the accumulating function in the inner foldl.
evaluateCoinChoice :: Map.Map Integer (Maybe (Int, [Integer])) -- DP map with solutions for smaller amounts.
                   -> Integer           -- The current amount for which a solution is being sought.
                   -> Maybe (Int, [Integer]) -- Accumulated best solution found so far for 'currentAmountToSolve'.
                   -> Integer           -- The coin denomination being considered.
                   -> Maybe (Int, [Integer]) -- Updated best solution after considering 'coinToTry'.
evaluateCoinChoice dpMapForSmallerAmounts currentAmountToSolve accBestSolutionForCurrentAmount coinToTry =
  if coinToTry <= currentAmountToSolve then
    -- If 'coinToTry' can be part of the solution (i.e., it's not larger than the amount):
    -- Look up the solution for the remaining amount: 'currentAmountToSolve - coinToTry'.
    case Map.lookup (currentAmountToSolve - coinToTry) dpMapForSmallerAmounts of
      Just (Just (prevCount, prevCoins)) -> -- A solution exists for the remaining amount.
        let newProposedCount = prevCount + 1
            newProposedCoins = coinToTry : prevCoins -- Prepend current coin.
        in case accBestSolutionForCurrentAmount of
             Nothing -> -- No solution found yet for 'currentAmountToSolve'; this is the first one.
               Just (newProposedCount, newProposedCoins)
             Just (existingBestCount, _) -> -- A solution already exists; check if this new one is better.
               if newProposedCount < existingBestCount then
                 -- The new way of making change uses fewer coins.
                 Just (newProposedCount, newProposedCoins)
               else
                 -- The existing solution is better or equally good in terms of coin count.
                 accBestSolutionForCurrentAmount
      _ -> -- No solution found for 'currentAmountToSolve - coinToTry'.
           -- So, 'coinToTry' cannot extend a previous solution to solve for 'currentAmountToSolve'.
           accBestSolutionForCurrentAmount
  else
    -- 'coinToTry' is larger than 'currentAmountToSolve', so it cannot be used.
    accBestSolutionForCurrentAmount
