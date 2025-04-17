module Change (findFewestCoins) where

import Data.List (minimumBy, sort)
import Data.Ord  (comparing)

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins
  | target < 0  = Nothing
  | target == 0 = Just []
  | otherwise   = dp !! fromIntegral target
  where
    -- Only consider positive coin values
    cs :: [Integer]
    cs = filter (> 0) coins

    -- Dynamic programming table: dp !! i = best combination for amount i
    dp :: [Maybe [Integer]]
    dp = [bestFor amt | amt <- [0 .. target]]

    bestFor :: Integer -> Maybe [Integer]
    bestFor 0 = Just []
    bestFor amt =
      let candidates =
            [ coin : combo
            | coin <- cs
            , let prev = amt - coin
            , prev >= 0
            , Just combo <- [dp !! fromIntegral prev]
            ]
      in case candidates of
           [] -> Nothing
           _  -> Just . sort $
                 minimumBy (comparing length) candidates
