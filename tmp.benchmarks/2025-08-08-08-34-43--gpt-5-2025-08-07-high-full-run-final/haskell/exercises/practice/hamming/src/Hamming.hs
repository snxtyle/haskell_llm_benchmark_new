module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys = go xs ys 0
  where
    go [] [] acc = Just acc
    go [] (_:_) _ = Nothing
    go (_:_) [] _ = Nothing
    go (a:as) (b:bs) acc =
      let acc' = if a /= b then acc + 1 else acc
       in go as bs acc'
