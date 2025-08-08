module Change (findFewestCoins) where

import Data.Array (Array, listArray, (!))
import Data.List (minimumBy, nub, sort)
import Data.Maybe (maybeToList)
import Data.Ord (comparing)

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins
  | target < 0 = Nothing
  | target == 0 = Just []
  | null coins' = Nothing
  | otherwise = fmap sort (dp ! target)
  where
    -- Use only positive, unique, sorted coin denominations
    coins' :: [Integer]
    coins' = sort . nub . filter (> 0) $ coins

    -- dp!i = best (fewest coins) list that sums to i, or Nothing if impossible
    dp :: Array Integer (Maybe [Integer])
    dp = listArray (0, target) (map best [0 .. target])

    best :: Integer -> Maybe [Integer]
    best 0 = Just []
    best i =
      let candidates =
            [ prev ++ [c]
            | c <- coins'
            , c <= i
            , let j = i - c
            , prev <- maybeToList (dp ! j)
            ]
      in case candidates of
           [] -> Nothing
           _  -> Just (minimumBy (comparing length) candidates)
