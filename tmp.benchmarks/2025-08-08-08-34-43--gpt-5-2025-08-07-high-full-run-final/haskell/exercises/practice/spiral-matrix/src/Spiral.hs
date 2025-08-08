module Spiral (spiral) where

spiral :: Int -> [[Int]]
spiral size
  | size <= 0 = []
  | otherwise = [[cell i j | j <- [0 .. size - 1]] | i <- [0 .. size - 1]]
  where
    cell i j =
      let k = minimum [i, j, size - 1 - i, size - 1 - j]
          s = size - 2 * k
          start = 1 + 4 * k * (size - k)
      in start + offset k s i j

    offset :: Int -> Int -> Int -> Int -> Int
    offset _ 1 _ _ = 0
    offset k s i j
      | i == k = j - k
      | j == jmax && i > k && i < imax = (s - 1) + (i - k)
      | i == imax = (2 * s - 2) + (jmax - j)
      | otherwise = (3 * s - 2) + (imax - 1 - i)
      where
        imax = k + s - 1
        jmax = k + s - 1
