module Queens (boardString, canAttack) where

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = unlines [row r | r <- [0..7]]
  where
    row r = concat [cell r c | c <- [0..7]] ++ " "
    cell r c = case (white, black) of
      (Just (wr, wc), _) | wr == r && wc == c -> "W "
      (_, Just (br, bc)) | br == r && bc == c -> "B "
      _ -> "_ "

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (row1, col1) (row2, col2) =
  row1 == row2 ||                     -- same row
  col1 == col2 ||                     -- same column
  abs (row1 - row2) == abs (col1 - col2)  -- same diagonal
