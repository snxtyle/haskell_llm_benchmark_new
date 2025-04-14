module Sublist (sublist) where

import Data.List (isInfixOf)

sublist :: Eq a => [a] -> [a] -> Maybe Ordering
sublist xs ys
  | xs == ys = Just EQ  -- Lists are equal
  | isInfixOf xs ys = Just LT  -- xs is a contiguous sublist of ys
  | isInfixOf ys xs = Just GT  -- xs is a contiguous superlist of ys (ys is a sublist of xs)
  | otherwise = Nothing  -- Neither
