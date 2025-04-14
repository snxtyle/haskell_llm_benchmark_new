module Change (findFewestCoins) where

import Data.Array (Array, listArray, (!))
import Data.Maybe (Maybe(..), catMaybes, listToMaybe)
import Data.List (sort, minimumBy)
import Data.Ord (comparing)

-- | Given a target amount and a list of coin denominations,
-- | find the list of coins with the minimum length that sums up to the target.
findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins
  -- Target cannot be negative
  | target < 0 = Nothing
  -- Target of 0 requires no coins
  | target == 0 = Just []
  | otherwise =
      -- Use only positive coin values, sorted (sorting helps consistency, not required for correctness here)
      let positiveCoins = sort $ filter (> 0) coins
          -- Get the smallest available coin
          smallestCoin = listToMaybe positiveCoins
      in case smallestCoin of
           -- If no positive coins are available and target > 0, impossible
           Nothing -> Nothing
           -- If target is positive but smaller than the smallest coin, impossible
           Just sc | target < sc -> Nothing
           -- Otherwise, proceed with dynamic programming calculation
           _ -> computeChange target positiveCoins

-- | Internal helper function using dynamic programming to compute the change.
computeChange :: Integer -> [Integer] -> Maybe [Integer]
computeChange target coins = solutions ! target
  where
    -- Define the bounds for our DP array (0 to target amount)
    bounds_ = (0, target)
    -- Create the DP array. Each element `i` stores `Maybe [Integer]`,
    -- representing the best coin list for amount `i`, or Nothing if impossible.
    -- The array is filled lazily based on the `solve` function.
    solutions :: Array Integer (Maybe [Integer])
    solutions = listArray bounds_ (map solve [0..target])

    -- | Computes the optimal coin list for a given amount 'amt'.
    solve :: Integer -> Maybe [Integer]
    -- Base case: Amount 0 requires an empty list of coins.
    solve 0 = Just []
    -- Recursive case: Compute solution for amount 'amt'.
    solve amt =
        -- Generate candidate solutions by trying each coin.
        let candidates = map (tryCoin amt) coins
            -- Filter out invalid candidates (where making change was impossible).
            validCandidates = catMaybes candidates
        -- Find the candidate list with the minimum number of coins.
        in findMin validCandidates

    -- | Tries using a specific 'coin' to reach amount 'amt'.
    -- | Returns `Just (coin : prevCoins)` if `solutions ! (amt - coin)` exists,
    -- | otherwise returns `Nothing`.
    tryCoin :: Integer -> Integer -> Maybe [Integer]
    tryCoin amt coin
      -- Cannot use a coin larger than the current amount.
      | coin > amt = Nothing
      | otherwise =
          -- Look up the best solution for the remaining amount (amt - coin).
          case solutions ! (amt - coin) of
            -- If no solution exists for the remainder, this coin cannot be used here.
            Nothing -> Nothing
            -- If a solution exists, prepend the current coin to it.
            Just prevCoins -> Just (coin : prevCoins)

    -- | Finds the list with the minimum length from a list of candidate lists.
    -- | Returns `Nothing` if the input list is empty.
    findMin :: [[Integer]] -> Maybe [Integer]
    findMin [] = Nothing
    -- Use minimumBy with comparing length to find the shortest list.
    findMin xs = Just $ minimumBy (comparing length) xs
