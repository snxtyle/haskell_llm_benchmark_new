module Minesweeper (annotate) where

annotate :: [String] -> [String]
annotate board = 
  let height = length board
      width = if null board then 0 else length (head board)

      -- Check if a position is within the board bounds
      inBounds r c = r >= 0 && r < height && c >= 0 && c < width

      -- Get the character at a given position
      getCell r c = (board !! r) !! c

      -- Count mines adjacent to (r,c)
      countMines r c = length [() | dr <- [-1..1], dc <- [-1..1], (dr,dc) /= (0,0),
                                   let nr = r + dr, let nc = c + dc,
                                   inBounds nr nc, getCell nr nc == '*']

      -- Annotate a single cell
      annotateCell r c =
        case getCell r c of
          '*' -> '*'
          ' ' -> let n = countMines r c in if n == 0 then ' ' else head (show n)
          ch  -> ch -- in case of unexpected char, keep as is

      -- Annotate a single row
      annotateRow r = [annotateCell r c | c <- [0..width-1]]

  in [annotateRow r | r <- [0..height-1]]
