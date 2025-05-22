module Spiral (spiral) where

spiral :: Int -> [[Int]]
spiral 0 = []
spiral size = 
    let matrix = [[0 | _ <- [1..size]] | _ <- [1..size]]
        filledMatrix = fillSpiral matrix size 1 0 0 0
    in filledMatrix

-- Fill the matrix in spiral order
-- matrix: current matrix state
-- size: matrix size
-- num: current number to place
-- row, col: current position
-- dir: direction (0=right, 1=down, 2=left, 3=up)
fillSpiral :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> [[Int]]
fillSpiral matrix size num row col dir
    | num > size * size = matrix
    | otherwise = 
        let newMatrix = setAt row col num matrix
            (newRow, newCol, newDir) = getNextPosition size newMatrix row col dir
        in fillSpiral newMatrix size (num + 1) newRow newCol newDir

-- Set value at specific position in matrix
setAt :: Int -> Int -> Int -> [[Int]] -> [[Int]]
setAt row col val matrix = 
    let (before, targetRow:after) = splitAt row matrix
        newRow = setAtList col val targetRow
    in before ++ [newRow] ++ after

-- Set value at specific position in list
setAtList :: Int -> Int -> [Int] -> [Int]
setAtList col val list =
    let (before, _:after) = splitAt col list
    in before ++ [val] ++ after

-- Get next position and direction
getNextPosition :: Int -> [[Int]] -> Int -> Int -> Int -> (Int, Int, Int)
getNextPosition size matrix row col dir =
    let directions = [(0, 1), (1, 0), (0, -1), (-1, 0)]  -- right, down, left, up
        (dRow, dCol) = directions !! dir
        nextRow = row + dRow
        nextCol = col + dCol
        
        -- Check if we need to turn (hit boundary or filled cell)
        needTurn = nextRow < 0 || nextRow >= size || 
                   nextCol < 0 || nextCol >= size ||
                   (matrix !! nextRow !! nextCol) /= 0
                   
    in if needTurn
       then let newDir = (dir + 1) `mod` 4
                (newDRow, newDCol) = directions !! newDir
            in (row + newDRow, col + newDCol, newDir)
       else (nextRow, nextCol, dir)
