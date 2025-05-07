module Spiral (spiral) where

spiral :: Int -> [[Int]]
spiral size = buildSpiral size 1

buildSpiral :: Int -> Int -> [[Int]]
buildSpiral n start
  | n <= 0    = []
  | n == 1    = [[start]]
  | otherwise =
      let
        end = start + 4 * (n - 1) - 1
        top    = [start .. start + n - 1]
        right  = [start + n .. start + 2 * n - 2]
        bottom = [start + 2 * n - 1 .. start + 3 * n - 3]
        left   = [start + 3 * n - 2 .. end]

        inner = buildSpiral (n - 2) (start + 4 * (n - 1))

        topRow = top
        middleRows = zipWith3 (\l mid r -> [l] ++ mid ++ [r]) (reverse left) inner right
        bottomRow = reverse bottom
      in
        if length inner == n - 2
          then [topRow] ++ middleRows ++ [bottomRow]
          else [topRow] ++ (map (\l -> [l]) (reverse left))
