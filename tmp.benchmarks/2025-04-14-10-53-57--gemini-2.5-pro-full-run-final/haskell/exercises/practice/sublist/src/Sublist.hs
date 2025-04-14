module Sublist (sublist) where

import Data.List (isInfixOf)

sublist :: Eq a => [a] -> [a] -> Maybe Ordering
sublist xs ys
  | xs == ys = Just EQ         -- Check for equality first
  | isInfixOf xs ys = Just LT  -- Check if xs is a sublist of ys
  | isInfixOf ys xs = Just GT  -- Check if ys is a sublist of xs (xs is a superlist)
  | otherwise = Nothing        -- Otherwise, they are unequal
