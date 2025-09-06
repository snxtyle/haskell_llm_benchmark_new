module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys
  | length xs /= length ys = Nothing
  | otherwise = Just $ length $ filter (\(x, y) -> x /= y) $ zip xs ys
