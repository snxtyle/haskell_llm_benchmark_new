module Change (findFewestCoins) where

import Data.List (sortBy)

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins
  | target < 0 = Nothing  -- Negative change is not possible
  | target == 0 = Just [] -- No coins needed for zero change
  | null coins = Nothing  -- No coins available to make change
  | target < minimum coins = Nothing  -- Target is smaller than smallest coin
  | otherwise = case dp target (sortDescending coins) of
      Nothing -> Nothing
      Just result -> Just (reverse result)
  where
    -- Sort coins in descending order for greedy approach optimization
    sortDescending = sortBy (flip compare)
    
    -- Dynamic programming solution to find minimum coins
    dp :: Integer -> [Integer] -> Maybe [Integer]
    dp 0 _ = Just []
    dp t cs = foldr firstValid Nothing [tryCoin c t cs | c <- cs]
    
    tryCoin :: Integer -> Integer -> [Integer] -> Maybe [Integer]
    tryCoin coin t cs
      | coin > t = Nothing
      | otherwise = case dp (t - coin) cs of
          Nothing -> Nothing
          Just rest -> Just (coin : rest)
    
    firstValid :: Maybe a -> Maybe a -> Maybe a
    firstValid (Just x) _ = Just x
    firstValid Nothing y = y
