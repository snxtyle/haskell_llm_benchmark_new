module Spiral (spiral) where

spiral :: Int -> [[Int]]
spiral 0 = []
spiral size = spiral' 1 0 0 size size (replicate size (replicate size 0))

spiral' :: Int -> Int -> Int -> Int -> Int -> [[Int]] -> [[Int]]
spiral' _ _ _ 0 _ matrix = matrix
spiral' _ _ _ _ 0 matrix = matrix
spiral' num row col height width matrix
  | height == 1 = updateRow num row col width matrix
  | width == 1 = updateColumn num row col height matrix
  | otherwise = 
      let matrix1 = updateRow num row col width matrix
          num1 = num + width - 1
          matrix2 = updateColumn num1 (row + 1) (col + width - 1) (height - 2) matrix1
          num2 = num1 + height - 2
          matrix3 = updateRow num2 (row + height - 1) (col + width - 2) (-(width - 1)) matrix2
          num3 = num2 + width - 1
          matrix4 = updateColumn num3 (row + height - 2) col (-(height - 3)) matrix3
          num4 = num3 + height - 3
      in spiral' num4 (row + 1) (col + 1) (height - 2) (width - 2) matrix4

updateRow :: Int -> Int -> Int -> Int -> [[Int]] -> [[Int]]
updateRow _ _ _ 0 matrix = matrix
updateRow num row col width matrix = 
  take row matrix ++ 
  [take col (matrix !! row) ++ 
   (if width > 0 
    then [num .. num + width - 1]
    else [num + width + 1 .. num]) ++ 
   drop (col + abs width) (matrix !! row)] ++ 
  drop (row + 1) matrix

updateColumn :: Int -> Int -> Int -> Int -> [[Int]] -> [[Int]]
updateColumn _ _ _ 0 matrix = matrix
updateColumn num row col height matrix =
  if height > 0
    then updateColumn' num row col height matrix
    else updateColumn' (num + height + 1) (row + height) col (abs height) matrix

updateColumn' :: Int -> Int -> Int -> Int -> [[Int]] -> [[Int]]
updateColumn' num row col height matrix =
  [let currentVal = matrix !! i !! col
   in if i >= row && i < row + height
      then take col (matrix !! i) ++ [num + i - row] ++ drop (col + 1) (matrix !! i)
      else matrix !! i | i <- [0..(length matrix - 1)]]
