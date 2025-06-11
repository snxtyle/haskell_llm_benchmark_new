module Change (findFewestCoins) where

import qualified Data.Map.Strict as M
import           Data.List       (minimumBy, sort)
import           Data.Ord        (comparing)

-- | Determine the fewest coins needed to make the given amount.
--
-- If it is impossible to make the exact change with the supplied
-- coins, this function returns Nothing.
--
-- The resulting list is returned in ascending order.
findFewestCoins :: Integer      -- ^ Target amount of change
                -> [Integer]    -- ^ Available coin denominations
                -> Maybe [Integer]
findFewestCoins target coins
  | target < 0           = Nothing          -- negative change is impossible
  | target == 0          = Just []          -- zero change needs no coins
  | null validCoins      = Nothing          -- no positive-valued coins
  | otherwise            = fmap sort $ M.lookup target dp
  where
    -- Remove any non-positive coin values as they don't make sense here.
    validCoins  = filter (> 0) coins
    coinsSorted = sort validCoins

    -- Dynamic programming table: amount ↦ minimal list of coins for that amount
    dp :: M.Map Integer [Integer]
    dp = foldl insertNext (M.singleton 0 []) [1 .. target]

    insertNext :: M.Map Integer [Integer] -> Integer -> M.Map Integer [Integer]
    insertNext table amt =
      case bestCombination of
        Nothing   -> table
        Just best -> M.insert amt best table
      where
        -- All possible ways to reach the current amount using one more coin
        possibilities =
          [ coin : prev
          | coin <- coinsSorted
          , coin <= amt
          , Just prev <- [M.lookup (amt - coin) table]
          ]

        -- Select the shortest combination, if any
        bestCombination =
          case possibilities of
            [] -> Nothing
            _  -> Just (minimumBy (comparing length) possibilities)
