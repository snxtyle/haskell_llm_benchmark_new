module Minesweeper (annotate) where

annotate :: [String] -> [String]
annotate [] = []
annotate board = map (processRow board) (zip [0..] board)
  where
    height = length board
    width = if null board then 0 else length (head board)
    
    -- Process each row with its row index
    processRow b (rowIdx, row) = map (processCell b rowIdx) (zip [0..] row)
    
    -- Process each cell with its column index
    processCell b rowIdx (colIdx, cell) = 
      if cell == '*' 
      then '*' 
      else if countAdjacentMines b rowIdx colIdx == 0 
           then ' ' 
           else head (show (countAdjacentMines b rowIdx colIdx))
    
    -- Count mines in the 8 adjacent cells
    countAdjacentMines b row col = 
      sum [if isMine b (row + dr) (col + dc) then 1 else 0
          | dr <- [-1..1], dc <- [-1..1], not (dr == 0 && dc == 0)]
    
    -- Check if a position contains a mine
    isMine b r c = 
      r >= 0 && r < height && c >= 0 && c < width && (b !! r) !! c == '*'
