module Spiral (spiral) where

spiral :: Int -> [[Int]]
spiral 0 = []
spiral 1 = [[1]]
spiral n =
  let ringSize = 4 * (n - 1)
      inner = spiral (n - 2)
      innerAdjusted = map (map (+ ringSize)) inner  -- Add offset to inner matrix numbers
      top = [1..n]  -- First segment (top row)
      -- Remaining segments
      rest = drop n [1..ringSize]
      (rs, remaining) = splitAt (n-1) rest
      (bs, ls') = splitAt (n-1) remaining
      bottom = reverse bs ++ [last rs]  -- Bottom row (reversed) and then add the last element of the right segment
      left = reverse ls'  -- Left column (reversed)
      -- Combine segments to form matrix
      topRow = top
      middleRows = if n>2
                   then [ [left !! i]   ++ (innerAdjusted !! i)   
                                 ++ [rs !! i] | i <- [0..n-3] ]
                   else []  -- No middle rows for n=2
      bottomRow = bottom
  in topRow : middleRows ++ [bottomRow]
