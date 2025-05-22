module Minesweeper (annotate) where

annotate :: [String] -> [String]
annotate board = [[annotateCell row col | col <- [0..width-1]] | row <- [0..height-1]]
  where
    height = length board
    width = if height > 0 then length (head board) else 0
    
    annotateCell :: Int -> Int -> Char
    annotateCell row col
      | getCell row col == '*' = '*'
      | mineCount == 0 = ' '
      | otherwise = head (show mineCount)
      where
        mineCount = countAdjacentMines row col
    
    getCell :: Int -> Int -> Char
    getCell row col
      | row < 0 || row >= height || col < 0 || col >= width = ' '
      | otherwise = (board !! row) !! col
    
    countAdjacentMines :: Int -> Int -> Int
    countAdjacentMines row col = length $ filter (== '*') adjacentCells
      where
        positions = [(r, c) | r <- [row-1..row+1], c <- [col-1..col+1], (r, c) /= (row, col)]
        adjacentCells = [getCell r c | (r, c) <- positions]
