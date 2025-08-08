module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys
  | length xs /= length ys = Nothing
  | otherwise = Just $ countDifferences xs ys
  where
    countDifferences :: String -> String -> Int
    countDifferences s1 s2 = length $ filter (uncurry (/=)) $ zip s1 s2
