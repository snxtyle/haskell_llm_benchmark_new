module Knapsack (maximumValue) where

-- Solves the 0-1 Knapsack problem using dynamic programming.
-- The approach uses a 1D DP table (represented as a list), where dp[w]
-- stores the maximum value that can be achieved with a knapsack of capacity w.
-- The table is updated iteratively for each item.
--
-- Complexity:
-- Time: O(num_items * capacity). For each item, we construct a new DP list.
--       List operations like splitAt, map, zipWith, and ++ take O(capacity) time.
-- Space: O(capacity) for storing the DP list.

maximumValue :: Int         -- ^ Knapsack's maximum weight capacity
             -> [(Int, Int)] -- ^ List of items, each as (weight, value)
             -> Int         -- ^ Maximum value of items that can be carried
maximumValue capacity items
  -- If capacity is negative, no items can be taken, so value is 0.
  | capacity < 0 = 0
  | otherwise =
    -- initialDp: DP table initialized to all zeros.
    -- dp[w] = 0 for all w from 0 to capacity.
    -- The size of the dp table is (capacity + 1).
    let initialDp = replicate (capacity + 1) 0

        -- processItem: Updates the DP table by considering a single item.
        -- currentDp: DP table from considering all previous items.
        -- (itemWeight, itemValue): The current item to consider.
        -- Returns: A new DP table that accounts for the current item.
        processItem :: [Int] -> (Int, Int) -> [Int]
        processItem currentDp (itemWeight, itemValue)
          -- Assuming item weights are positive. If itemWeight <= 0,
          -- it could lead to issues (e.g., infinite value if itemValue > 0,
          -- or errors if itemWeight is negative for splitAt).
          -- Problem statement says values are strictly positive.
          -- Standard knapsack problems usually assume positive weights.
          | itemWeight <= 0 = currentDp
          | otherwise =
              -- The DP update rule for item i and capacity w is:
              --   dp_new[w] = max(dp_old[w], itemValue + dp_old[w - itemWeight])
              -- `currentDp` is dp_old. We are constructing dp_new.

              -- `dpPrefix` corresponds to dp_new[w] for w < itemWeight.
              -- For these capacities, the current item cannot be taken (it's too heavy).
              -- So, dp_new[w] = dp_old[w]. These are the first `itemWeight` elements of `currentDp`.
              -- `dpSuffix` corresponds to dp_old[w] for w >= itemWeight.
              let (dpPrefix, dpSuffix) = splitAt itemWeight currentDp
                  
                  -- `comparandsFromPrevDp` are the dp_old[w - itemWeight] values.
                  -- These are needed to calculate the value if the current item is taken.
                  -- Specifically, for each dpSuffix[j] (which is dp_old[itemWeight + j]),
                  -- the corresponding dp_old[itemWeight + j - itemWeight] is dp_old[j].
                  -- So, comparandsFromPrevDp are currentDp[0], currentDp[1], ..., currentDp[capacity - itemWeight].
                  -- The number of such elements is `length dpSuffix`.
                  comparandsFromPrevDp = take (length dpSuffix) currentDp
                  
                  -- `valuesIfItemTaken` represents `itemValue + dp_old[w - itemWeight]` for w >= itemWeight.
                  valuesIfItemTaken = map (+ itemValue) comparandsFromPrevDp
                  
                  -- `updatedSuffix` represents dp_new[w] for w >= itemWeight.
                  -- For each such w, we choose between not taking the item (value is dpSuffix[j])
                  -- or taking the item (value is valuesIfItemTaken[j]).
                  updatedSuffix = zipWith max dpSuffix valuesIfItemTaken
                  
              -- The new DP table is the prefix (unchanged) concatenated with the updated suffix.
              in dpPrefix ++ updatedSuffix

        -- finalDp: The DP table after processing all items.
        -- Achieved by folding `processItem` over the list of items, starting with `initialDp`.
        finalDp = foldl processItem initialDp items

    -- The result is the value in the DP table for the full knapsack capacity.
    -- If finalDp is empty (e.g. if capacity was < 0, though guarded), result is 0.
    -- Otherwise, it's the last element if capacity is appropriately indexed,
    -- or `finalDp !! capacity` if list is 0-indexed up to `capacity`.
    in if null finalDp
       then 0
       else finalDp !! capacity
