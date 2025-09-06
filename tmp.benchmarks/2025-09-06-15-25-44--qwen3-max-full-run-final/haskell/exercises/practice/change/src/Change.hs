module Change (findFewestCoins) where

import Data.Array (Array, listArray, (!))
import Data.Maybe (mapMaybe)
import Data.List (sort)

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins
  | target < 0 = Nothing
  | target == 0 = Just []
  | null validCoins = Nothing
  | otherwise = computeChange target (sort validCoins)
  where
    validCoins = filter (> 0) coins

computeChange :: Integer -> [Integer] -> Maybe [Integer]
computeChange target coins = go target []
  where
    -- Create a DP table: min coins needed for each amount from 0 to target
    dp :: Array Integer (Maybe Integer)
    dp = listArray (0, target) $ map computeDP [0..target]
      where
        computeDP 0 = Just 0
        computeDP i = minimumMaybe $ mapMaybe tryCoin coins
          where
            tryCoin coin
              | coin <= i, dp ! (i - coin) /= Nothing = 
                  fmap (+1) (dp ! (i - coin))
              | otherwise = Nothing
    
    -- Reconstruct the actual coins used
    go :: Integer -> [Integer] -> Maybe [Integer]
    go 0 acc = Just acc
    go remaining acc
      | remaining < 0 = Nothing
      | dp ! remaining == Nothing = Nothing
      | otherwise = case findWorkingCoin of
          Just coin -> go (remaining - coin) (coin : acc)
          Nothing -> Nothing
      where
        findWorkingCoin = find (\coin -> 
          coin <= remaining && 
          dp ! (remaining - coin) == fmap (subtract 1) (dp ! remaining)) coins
    
    find :: (a -> Bool) -> [a] -> Maybe a
    find _ [] = Nothing
    find p (x:xs) = if p x then Just x else find p xs
    
    minimumMaybe :: Ord a => [Maybe a] -> Maybe a
    minimumMaybe [] = Nothing
    minimumMaybe xs = case filter (/= Nothing) xs of
      [] -> Nothing
      ys -> Just $ minimum $ mapMaybe id ys
