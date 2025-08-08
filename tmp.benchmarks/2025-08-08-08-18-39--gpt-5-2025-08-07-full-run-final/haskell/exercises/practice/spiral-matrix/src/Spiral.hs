module Spiral (spiral) where

spiral :: Int -> [[Int]]
spiral n
  | n <= 0    = []
  | n == 1    = [[1]]
  | otherwise =
      let k = n
          top = [1 .. k]
          right = [k + 1 .. k + (k - 1)]
          bottom = [k + (k - 1) + 1 .. k + 2 * (k - 1)]
          left = [k + 2 * (k - 1) + 1 .. k + 2 * (k - 1) + (k - 2)]
          offset = k + 2 * (k - 1) + (k - 2)
          inner = map (map (+ offset)) (spiral (k - 2))

          topRow = top
          rightMid = take (k - 2) right
          leftRev = reverse left

          middleRows =
            [ [leftRev !! i] ++ innerRow ++ [rightMid !! i]
            | i <- [0 .. k - 3]
            , let innerRow = inner !! i
            ]

          bottomRow = reverse bottom ++ [last right]
       in topRow : middleRows ++ [bottomRow]
