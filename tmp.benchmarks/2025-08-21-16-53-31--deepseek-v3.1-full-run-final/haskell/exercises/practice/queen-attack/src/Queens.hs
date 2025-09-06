module Queens (boardString, canAttack) where

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = unlines $ map (generateRow white black) [0..7]
  where
    generateRow w b row = unwords $ map (generateCell w b row) [0..7]
    generateCell w b row col
      | Just (row, col) == w = "W"
      | Just (row, col) == b = "B"
      | otherwise = "_"

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (x1, y1) (x2, y2) =
  x1 == x2 ||          -- Same row
  y1 == y2 ||          -- Same column
  abs (x1 - x2) == abs (y1 - y2)  -- Same diagonal
