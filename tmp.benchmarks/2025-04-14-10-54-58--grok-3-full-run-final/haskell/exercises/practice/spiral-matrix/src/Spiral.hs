module Spiral (spiral) where

spiral :: Int -> [[Int]]
spiral size
  | size <= 0 = []
  | size == 1 = [[1]]
  | otherwise = buildSpiral size 1
  where
    buildSpiral n start =
      if n == 1 then [[start]]
      else let top = [start .. start + n - 1]
               right = [start + n .. start + 2*n - 2]
               bottom = reverse [start + 2*n - 1 .. start + 3*n - 3]
               left = reverse [start + 3*n - 2 .. start + 4*n - 4]
               inner = buildSpiral (n - 2) (start + 4*n - 4)
               middleRows = zipWith3 (\r i l -> r : i ++ [l]) right inner left
           in top : middleRows ++ [bottom]
