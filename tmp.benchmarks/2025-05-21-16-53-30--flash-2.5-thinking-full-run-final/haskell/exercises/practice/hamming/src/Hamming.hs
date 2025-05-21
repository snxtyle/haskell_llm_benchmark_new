module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys
  | length xs /= length ys = Nothing
  | otherwise              = Just (countDifferences (zip xs ys))
  where
    countDifferences :: [(Char, Char)] -> Int
    countDifferences = length . filter (\(x, y) -> x /= y)
