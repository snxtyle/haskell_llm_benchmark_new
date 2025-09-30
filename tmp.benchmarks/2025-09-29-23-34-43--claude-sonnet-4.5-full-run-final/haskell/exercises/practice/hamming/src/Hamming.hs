module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys
  | length xs /= length ys = Nothing
  | otherwise = Just $ countDifferences xs ys
  where
    countDifferences [] [] = 0
    countDifferences (x:xs') (y:ys')
      | x == y    = countDifferences xs' ys'
      | otherwise = 1 + countDifferences xs' ys'
    countDifferences _ _ = 0
