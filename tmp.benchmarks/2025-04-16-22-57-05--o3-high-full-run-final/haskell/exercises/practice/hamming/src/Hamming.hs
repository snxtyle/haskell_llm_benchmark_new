module Hamming (distance) where

-- | Calculate the Hamming distance between two DNA strands.
--   If the strands are of unequal length, return Nothing.
distance :: String -> String -> Maybe Int
distance xs ys = go xs ys 0
  where
    go :: String -> String -> Int -> Maybe Int
    go [] [] acc         = Just acc
    go (_:_) [] _        = Nothing
    go [] (_:_) _        = Nothing
    go (a:as) (b:bs) acc =
      let acc' = if a /= b then acc + 1 else acc
       in go as bs acc'
