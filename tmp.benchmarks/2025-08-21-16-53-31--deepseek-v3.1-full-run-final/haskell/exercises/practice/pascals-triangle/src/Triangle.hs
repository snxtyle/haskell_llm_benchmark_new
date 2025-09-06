module Triangle (rows) where

rows :: Int -> [[Integer]]
rows 0 = []
rows n = take n triangle
  where
    triangle = [1] : map nextRow triangle
    nextRow prev = zipWith (+) (0:prev) (prev ++ [0])
