module Change (findFewestCoins) where

import Data.List (sort)

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins
  | target < 0 = Nothing
  | target == 0 = Just []
  | null coins = Nothing
  | otherwise = findMinCoins target (sort coins)

findMinCoins :: Integer -> [Integer] -> Maybe [Integer]
findMinCoins target coins = 
  let dp = buildDP target coins
  in case dp !! fromIntegral target of
       (count, path) | count <= target -> Just (reverse path)
       _ -> Nothing

buildDP :: Integer -> [Integer] -> [(Integer, [Integer])]
buildDP target coins = dp
  where
    dp = scanl1 seq $ map computeMin [0..target]
    
    computeMin :: Integer -> (Integer, [Integer])
    computeMin 0 = (0, [])
    computeMin amount = 
      let candidates = [(count + 1, coin : path) | coin <- coins,
                                                    coin <= amount,
                                                    let (count, path) = dp !! fromIntegral (amount - coin),
                                                    count < amount]
      in if null candidates
         then (amount + 1, [])  -- Impossible
         else minimum candidates
