module Change (findFewestCoins) where

import qualified Data.Array as A
import           Data.List  (minimumBy, sort)
import           Data.Ord   (comparing)

-- | Given a target amount and a list of coin values, return the combination
-- of coins (as a list) that adds up to the target using the fewest coins
-- possible.  If no such combination exists, return Nothing.
--
-- The returned list is sorted in ascending order for deterministic output.
findFewestCoins :: Integer      -- ^ Target amount
                -> [Integer]    -- ^ Available coin denominations
                -> Maybe [Integer]
findFewestCoins target coins
  | target < 0            = Nothing              -- negative change not allowed
  | target == 0           = Just []              -- zero change requires no coins
  | null positiveCoins    = Nothing              -- no usable coins
  | otherwise             = fmap (sort . map toInteger) best
  where
    -- Keep only positive coin values and sort them to aid determinism
    positiveCoins :: [Integer]
    positiveCoins = sort (filter (> 0) coins)

    -- Convert to Int for array indices (assumes target fits in Int)
    tInt     :: Int
    tInt     = fromIntegral target

    coinsInt :: [Int]
    coinsInt = map fromIntegral positiveCoins

    -- Dynamic‑programming table: dp!n is the optimal list of coins for amount n
    dp :: A.Array Int (Maybe [Int])
    dp = A.listArray (0, tInt) (map bestFor [0 .. tInt])

    best :: Maybe [Int]
    best = dp A.! tInt

    -- Determine the optimal coin list for a particular amount
    bestFor :: Int -> Maybe [Int]
    bestFor 0 = Just []
    bestFor amt =
      let candidates =
            [ coin : prev
            | coin <- coinsInt
            , coin <= amt
            , Just prev <- [dp A.! (amt - coin)]
            ]
      in if null candidates
           then Nothing
           else Just (minimumBy (comparing length) candidates)
