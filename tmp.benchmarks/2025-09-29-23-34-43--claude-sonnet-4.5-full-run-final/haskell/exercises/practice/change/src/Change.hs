module Change (findFewestCoins) where

import Data.List (sortBy)
import Data.Ord (comparing)

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins
  | target < 0 = Nothing
  | target == 0 = Just []
  | null coins = Nothing
  | otherwise = findCoins target (sortBy (comparing negate) coins)

findCoins :: Integer -> [Integer] -> Maybe [Integer]
findCoins target coins = 
  let dp = buildDP target coins
  in if dp !! fromIntegral target == maxBound
     then Nothing
     else Just (reconstructCoins target coins dp)

-- Build DP table where dp[i] = minimum number of coins to make amount i
buildDP :: Integer -> [Integer] -> [Int]
buildDP target coins = dp
  where
    dp = 0 : [minCoins i | i <- [1..target]]
    
    minCoins :: Integer -> Int
    minCoins amount = minimum candidates
      where
        candidates = maxBound : 
          [ 1 + dp !! fromIntegral (amount - coin)
          | coin <- coins
          , coin <= amount
          , let prevCount = dp !! fromIntegral (amount - coin)
          , prevCount /= maxBound
          ]

-- Reconstruct the actual coins used
reconstructCoins :: Integer -> [Integer] -> [Int] -> [Integer]
reconstructCoins 0 _ _ = []
reconstructCoins amount coins dp = 
  let validCoins = 
        [ coin
        | coin <- coins
        , coin <= amount
        , let prevIdx = fromIntegral (amount - coin)
        , prevIdx >= 0
        , prevIdx < length dp
        , dp !! prevIdx /= maxBound
        , dp !! fromIntegral amount == 1 + dp !! prevIdx
        ]
  in case validCoins of
       (coin:_) -> coin : reconstructCoins (amount - coin) coins dp
       [] -> []
