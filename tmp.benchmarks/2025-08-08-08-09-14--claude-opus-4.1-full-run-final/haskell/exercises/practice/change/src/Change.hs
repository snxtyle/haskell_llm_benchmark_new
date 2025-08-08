module Change (findFewestCoins) where

import Data.List (sort)
import Data.Maybe (isJust)

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins
  | target < 0 = Nothing
  | target == 0 = Just []
  | null coins = Nothing
  | otherwise = findMinCoins target (sort coins)

findMinCoins :: Integer -> [Integer] -> Maybe [Integer]
findMinCoins target sortedCoins = 
  let dp = buildDP target sortedCoins
  in case lookup target dp of
       Just (_, result) -> result
       Nothing -> Nothing

buildDP :: Integer -> [Integer] -> [(Integer, (Int, Maybe [Integer]))]
buildDP target coins = dp
  where
    dp = (0, (0, Just [])) : [(i, findBest i) | i <- [1..target]]
    
    findBest :: Integer -> (Int, Maybe [Integer])
    findBest amount = 
      let candidates = [(count + 1, fmap (coin:) coinList) 
                       | coin <- coins
                       , coin <= amount
                       , let prevAmount = amount - coin
                       , Just (count, Just coinList) <- [lookup prevAmount dp]]
      in if null candidates
         then (maxBound, Nothing)
         else minimumPair candidates

minimumPair :: [(Int, Maybe [Integer])] -> (Int, Maybe [Integer])
minimumPair [] = (maxBound, Nothing)
minimumPair xs = foldr1 minPair xs
  where
    minPair (c1, m1) (c2, m2)
      | isJust m1 && isJust m2 = if c1 <= c2 then (c1, m1) else (c2, m2)
      | isJust m1 = (c1, m1)
      | isJust m2 = (c2, m2)
      | otherwise = (maxBound, Nothing)
