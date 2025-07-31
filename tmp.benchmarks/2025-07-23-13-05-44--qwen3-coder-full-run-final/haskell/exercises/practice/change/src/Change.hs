module Change (findFewestCoins) where

import Data.List (sort, find)

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins
  | target < 0 = Nothing  -- Can't make negative change
  | target == 0 = Just [] -- No coins needed for zero change
  | otherwise = case coins of
      [] -> Nothing -- No coins available
      _ -> if minimum coins > target
           then Nothing -- Smallest coin is larger than target
           else findMinCoins target (sort (filter (>0) coins))

-- Dynamic programming approach to find minimum coins
findMinCoins :: Integer -> [Integer] -> Maybe [Integer]
findMinCoins target coins = 
  let dp = buildDPTable target coins
      result = reconstructSolution target coins dp
  in case result of
      [] -> if target == 0 then Just [] else Nothing
      _ -> Just (sort result)

-- Build DP table where dp[i] is the minimum number of coins needed for amount i
buildDPTable :: Integer -> [Integer] -> [Maybe Int]
buildDPTable target coins = dp
  where
    dp = [calculateMin coins i dp | i <- [0..target]]
    
    calculateMin _ 0 _ = Just 0
    calculateMin cs amount table = 
      let validCoins = filter (<= amount) cs
          possibleSolutions = [table !! fromIntegral (amount - coin) | coin <- validCoins]
          validSolutions = [x | Just x <- possibleSolutions]
      in if null validSolutions 
         then Nothing 
         else Just (1 + minimum validSolutions)

-- Reconstruct the actual coins used in the optimal solution
reconstructSolution :: Integer -> [Integer] -> [Maybe Int] -> [Integer]
reconstructSolution 0 _ _ = []
reconstructSolution target coins dp = 
  let validCoins = filter (<= target) coins
      -- Find a coin that leads to an optimal solution
      bestCoin = find (\coin -> 
        let prevAmount = target - coin
            prevMin = dp !! fromIntegral prevAmount
            currentMin = dp !! fromIntegral target
        in case (prevMin, currentMin) of
            (Just p, Just c) -> c == p + 1
            _ -> False
        ) validCoins
  in case bestCoin of
      Nothing -> []
      Just coin -> coin : reconstructSolution (target - coin) coins dp
