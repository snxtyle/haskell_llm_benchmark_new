module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys
  | length xs /= length ys = Nothing
  | otherwise              = Just $ countDifferences xs ys
  where
    countDifferences :: String -> String -> Int
    countDifferences as bs = length $ filter id $ zipWith (/=) as bs
