module Minesweeper (annotate) where

annotate :: [String] -> [String]
annotate board = map (annotateRow board) (zip [0..] board)
  where
    rows = length board
    cols = if rows == 0 then 0 else length (head board)
    
    annotateRow :: [String] -> (Int, String) -> String
    annotateRow b (rowIdx, row) = map (annotateCell b rowIdx) (zip [0..] row)
    
    annotateCell :: [String] -> Int -> (Int, Char) -> Char
    annotateCell b rowIdx (colIdx, cell)
      | cell == '*' = '*'
      | cell == ' ' = 
          let count = countAdjacentMines b rowIdx colIdx
          in if count == 0 then ' ' else head (show count)
      | otherwise = cell
    
    countAdjacentMines :: [String] -> Int -> Int -> Int
    countAdjacentMines b rowIdx colIdx = 
      length $ filter isMine adjacentPositions
      where
        adjacentPositions = [(r, c) | r <- [rowIdx-1..rowIdx+1], 
                                     c <- [colIdx-1..colIdx+1],
                                     (r, c) /= (rowIdx, colIdx),
                                     r >= 0, r < rows,
                                     c >= 0, c < cols]
        
        isMine :: (Int, Int) -> Bool
        isMine (r, c) = (b !! r) !! c == '*'
