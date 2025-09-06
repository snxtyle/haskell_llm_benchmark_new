module Spiral (spiral) where

spiral :: Int -> [[Int]]
spiral 0 = []
spiral n = spiral' n 1
  where
    spiral' :: Int -> Int -> [[Int]]
    spiral' 0 _ = []
    spiral' 1 start = [[start]]
    spiral' 2 start = [[start, start + 1], [start + 3, start + 2]]
    spiral' size start =
      let top = [start .. start + size - 1]
          right = [start + size .. start + 2 * size - 2]
          bottom = reverse [start + 2 * size - 1 .. start + 3 * size - 3]
          left = reverse [start + 3 * size - 2 .. start + 4 * size - 4]
          inner = spiral' (size - 2) (start + 4 * size - 4)
      in top : zipWith3 (\r row l -> r : row ++ [l]) right inner left ++ [bottom]
