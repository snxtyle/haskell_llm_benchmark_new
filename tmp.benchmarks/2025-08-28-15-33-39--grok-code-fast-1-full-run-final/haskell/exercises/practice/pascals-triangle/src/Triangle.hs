module Triangle (rows) where

rows :: Int -> [[Integer]]
rows n = take n (iterate nextRow [1])
  where
    nextRow xs = 1 : zipWith (+) xs (drop 1 xs) ++ [1]
