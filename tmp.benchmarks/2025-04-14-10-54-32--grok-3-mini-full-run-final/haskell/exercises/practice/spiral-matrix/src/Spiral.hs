module Spiral (spiral) where

spiral :: Int -> [[Int]]
spiral n
  | n <= 0 = []
  | otherwise = foldl updatePosition initialMatrix (zip positions [1..n*n])
  where
    initialMatrix = replicate n (replicate n 0)  -- n x n matrix of zeros
    positions = generatePositions 0 (n-1) 0 (n-1)

generatePositions :: Int -> Int -> Int -> Int -> [(Int, Int)]
generatePositions startRow endRow startCol endCol
  | startRow > endRow || startCol > endCol = []
  | otherwise = topRow ++ rightCol ++ bottomRow ++ leftCol ++ generatePositions (startRow + 1) (endRow - 1) (startCol + 1) (endCol - 1)
  where
    topRow = [(startRow, j) | j <- [startCol .. endCol]]
    rightCol = [(i, endCol) | i <- [startRow + 1 .. endRow]]
    bottomRow = if startRow < endRow then [(endRow, j) | j <- reverse [startCol .. endCol - 1]] else []
    leftCol = if startCol < endCol then [(i, startCol) | i <- reverse [startRow + 1 .. endRow - 1]] else []

updatePosition :: [[Int]] -> ((Int, Int), Int) -> [[Int]]
updatePosition matrix ((i, j), num) = take i matrix ++ [newRow] ++ drop (i + 1) matrix
  where
    row = matrix !! i  -- Get the i-th row
    newRow = take j row ++ [num] ++ drop (j + 1) row  -- Update the j-th element in that row
