module Hamming (distance) where

-- | Calculate the Hamming distance between two DNA strands.
--   If the strands are of unequal length, return Nothing.
distance :: String -> String -> Maybe Int
distance xs ys = go xs ys 0
  where
    go []     []     acc = Just acc               -- same length, finished
    go (_:_)  []     _   = Nothing                -- xs longer than ys
    go []     (_:_)  _   = Nothing                -- ys longer than xs
    go (a:as) (b:bs) acc =
      let acc' = if a /= b then acc + 1 else acc  -- increment when mismatch
      in go as bs acc'
