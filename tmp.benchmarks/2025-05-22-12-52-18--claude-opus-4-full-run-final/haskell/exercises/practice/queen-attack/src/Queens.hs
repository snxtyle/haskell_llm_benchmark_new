module Queens (boardString, canAttack) where

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = unlines $ map rowString [0..7]
  where
    rowString row = unwords $ map (cellString row) [0..7]
    cellString row col
      | Just (row, col) == white = "W"
      | Just (row, col) == black = "B"
      | otherwise = "_"

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (row1, col1) (row2, col2) =
  row1 == row2 ||                    -- Same row
  col1 == col2 ||                    -- Same column
  abs (row1 - row2) == abs (col1 - col2)  -- Same diagonal
