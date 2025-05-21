module Change (findFewestCoins) where

import Data.List (sort, filter, minimumBy, foldl') -- Added foldl'
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (comparing) -- For minimumBy

-- | findFewestCoins calculates the fewest number of coins to make a given target amount.
--
-- Arguments:
--   target :: Integer - The amount of change to make.
--   coins  :: [Integer] - A list of available coin denominations.
--
-- Returns:
--   Maybe [Integer] - A list of coins that sum up to the target with the minimum count,
--                     or Nothing if the target cannot be made or is negative.
findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins
  | target < 0 = Nothing -- Cannot make negative change
  | target == 0 = Just [] -- 0 coins for 0 amount
  | otherwise =
      let
        -- Filter out non-positive coins and sort them.
        -- Sorting is not strictly necessary for correctness but can be good practice.
        -- It also ensures that if multiple solutions have the same minimum count,
        -- the one with smaller coins first (if added in order) is chosen,
        -- though the problem doesn't specify tie-breaking for the output list order.
        validCoins = sort $ filter (> 0) coins

        -- dpTable: Map from amount to (count, coin_list)
        -- `Map Integer (Maybe (Int, [Integer]))`
        -- `Nothing` means not reachable or not yet computed.
        -- `Just (count, coins)` means reachable with `count` coins and `coins` list.
        --
        -- The base case is that 0 amount needs 0 coins.
        initialDp :: Map Integer (Maybe (Int, [Integer]))
        initialDp = Map.singleton 0 (Just (0, []))

        -- `foldl'` is used to build the DP table iteratively from 1 up to `target`.
        -- It's strict, which helps in avoiding space leaks.
        dpTable :: Map Integer (Maybe (Int, [Integer]))
        dpTable = foldl' calculateMinCoins initialDp [1 .. target]

        -- Helper function to calculate the minimum coins for a given amount `n`.
        -- `currentDp` is the DP table built so far (for amounts less than `n`).
        calculateMinCoins :: Map Integer (Maybe (Int, [Integer])) -> Integer -> Map Integer (Maybe (Int, [Integer]))
        calculateMinCoins currentDp n =
          let
            -- Try to find the best solution for amount `n` by considering each `coin`
            -- in `validCoins`. For each `coin`, we look up the solution for `n - coin`
            -- in `currentDp`.
            possibleSolutions :: [ (Int, [Integer]) ]
            possibleSolutions =
              [ (count + 1, c : cs) -- Add 1 to count and prepend current coin 'c'
              | c <- validCoins
              , let remaining = n - c
              , remaining >= 0 -- Ensure remaining amount is non-negative
              -- Look up the solution for `remaining` in the `currentDp` table.
              -- `Map.findWithDefault Nothing` handles cases where `remaining` is not in the map.
              , Just (count, cs) <- [Map.findWithDefault Nothing remaining currentDp]
              ]

            -- Find the solution with the minimum number of coins from `possibleSolutions`.
            bestSolution :: Maybe (Int, [Integer])
            bestSolution =
              if null possibleSolutions
                then Nothing -- No solution found for amount `n`
                else Just $ minimumBy (comparing fst) possibleSolutions -- Compare by count (fst element of tuple)

          in
            -- Insert the best solution found for `n` into the DP table.
            Map.insert n bestSolution currentDp

      in
        -- After building the full dpTable up to `target`,
        -- retrieve the result for `target`.
        -- If `dpTable` contains `Just (count, coins)`, return `Just coins`.
        -- Otherwise, return `Nothing`.
        fmap snd (Map.findWithDefault Nothing target dpTable)
