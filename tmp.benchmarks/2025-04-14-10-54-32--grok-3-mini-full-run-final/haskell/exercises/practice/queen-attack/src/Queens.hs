module Queens (boardString, canAttack) where

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString mWhite mBlack =
    let grid = replicate 8 (replicate 8 '_')  -- 8x8 grid of underscores
        gridWithQueens = addQueen grid mWhite 'W'
        finalGrid = addQueen gridWithQueens mBlack 'B'
        rowsStr = [ unwords [ [finalGrid !! i !! j] | j <- [0..7] ] | i <- [0..7] ]
    in unlines rowsStr
  where
    addQueen grid Nothing _ = grid  -- No queen to add
    addQueen grid (Just (row, col)) char =
        take row grid ++ [take col (grid !! row) ++ [char] ++ drop (col + 1) (grid !! row)] ++ drop (row + 1) grid

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (row1, col1) (row2, col2) =
    row1 == row2 ||  -- Same row
    col1 == col2 ||  -- Same column
    abs (row1 - row2) == abs (col1 - col2)  -- Same diagonal
