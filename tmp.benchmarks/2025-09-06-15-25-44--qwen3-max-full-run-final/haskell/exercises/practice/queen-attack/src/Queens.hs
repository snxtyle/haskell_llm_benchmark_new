module Queens (boardString, canAttack) where

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = unlines [renderRow row | row <- [0..7]]
  where
    renderRow :: Int -> String
    renderRow row = unwords [renderSquare row col | col <- [0..7]]
    
    renderSquare :: Int -> Int -> String
    renderSquare row col
      | Just (r, c) <- white, r == row && c == col = "W"
      | Just (r, c) <- black, r == row && c == col = "B"
      | otherwise = "_"

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (row1, col1) (row2, col2) =
  row1 == row2 ||                    -- same row
  col1 == col2 ||                    -- same column
  abs (row1 - row2) == abs (col1 - col2)  -- same diagonal
