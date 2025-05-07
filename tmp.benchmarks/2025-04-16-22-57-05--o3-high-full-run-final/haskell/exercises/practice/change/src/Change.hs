module Change (findFewestCoins) where

import Data.List (sort, minimumBy)
import Data.Ord  (comparing)

-- | Determine the fewest coins needed to reach the given target.
--   Returns  'Nothing' when no combination is possible.
--
--   The resulting list is sorted in non‑decreasing order.
--
--   Examples:
--
--   >>> findFewestCoins 15 [1,5,10,25,100]
--   Just [5,10]
--
--   >>> findFewestCoins 40 [1,5,10,25,100]
--   Just [5,10,25]
--
findFewestCoins :: Integer       -- ^ target amount
                -> [Integer]     -- ^ list of coin values
                -> Maybe [Integer]
findFewestCoins target coins
  | target < 0      = Nothing             -- negative targets are impossible
  | target == 0     = Just []             -- zero needs no coins
  | null validCoins = Nothing             -- no positive‑value coins provided
  | otherwise       = table !! idx target
  where
    -- keep only positive coin values and sort them
    validCoins :: [Integer]
    validCoins = sort (filter (> 0) coins)

    -- convert an Integer index to Int (safe for typical exercise ranges)
    idx :: Integer -> Int
    idx = fromIntegral

    -- Dynamic programming table:
    -- table !! n  ==  the optimal combination for amount = n
    table :: [Maybe [Integer]]
    table = map best [0 .. target]

    best :: Integer -> Maybe [Integer]
    best 0 = Just []
    best amt =
      chooseMin
        [ coin : prev
        | coin <- validCoins
        , coin <= amt
        , Just prev <- [table !! idx (amt - coin)]
        ]

    chooseMin :: [[Integer]] -> Maybe [Integer]
    chooseMin [] = Nothing
    chooseMin xs = Just (minimumBy (comparing length) xs)
