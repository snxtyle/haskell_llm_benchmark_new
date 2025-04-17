module Hamming (distance) where

-- | Compute the Hamming distance between two DNA strands.
--   Returns @Nothing@ when the strands are of unequal length,
--   otherwise returns @Just n@ where @n@ is the number of differing
--   positions.
--
--   Examples:
--
--   >>> distance "GAGCCTACTAACGGGAT" "CATCGTAATGACGGCCT"
--   Just 7
--
--   >>> distance "GGACTGA" "GGACTGA"
--   Just 0
--
--   >>> distance "A" "GG"
--   Nothing
distance :: String -> String -> Maybe Int
distance xs ys
  | length xs /= length ys = Nothing
  | otherwise              = Just $ countDiffs xs ys
  where
    -- Count differing characters position‑by‑position.
    countDiffs :: String -> String -> Int
    countDiffs as bs = length . filter id $ zipWith (/=) as bs
