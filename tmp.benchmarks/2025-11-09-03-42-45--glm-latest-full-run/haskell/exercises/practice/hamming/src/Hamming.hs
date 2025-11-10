module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys
  | length xs /= length ys = Nothing
  | otherwise = Just (sum (map countMismatch (zip xs ys)))
  where
    countMismatch (a, b) = if a /= b then 1 else 0
