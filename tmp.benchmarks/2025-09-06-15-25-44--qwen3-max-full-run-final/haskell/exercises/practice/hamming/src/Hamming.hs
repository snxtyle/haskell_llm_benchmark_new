module Hamming (distance) where

import Data.Maybe (Maybe(..))

distance :: String -> String -> Maybe Int
distance xs ys
  | length xs /= length ys = Nothing
  | otherwise = Just (countDifferences xs ys)
  where
    countDifferences [] [] = 0
    countDifferences (x:xs) (y:ys)
      | x /= y    = 1 + countDifferences xs ys
      | otherwise = countDifferences xs ys
