module Queens (boardString, canAttack) where

-- | Determines if two queens can attack each other.
-- Queens attack if they are on the same row, column, or diagonal.
canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (r1, c1) (r2, c2) =
    sameRow || sameCol || sameDiag
  where
    sameRow = r1 == r2
    sameCol = c1 == c2
    sameDiag = abs (r1 - r2) == abs (c1 - c2)

-- | Generates a string representation of the chess board.
-- Places 'W' for the white queen, 'B' for the black queen, and '_' for empty squares.
-- Each square is separated by a space.
boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = unlines [ unwords [ [cellChar (r, c)] | c <- [0..7]] | r <- [0..7] ]
  where
    cellChar :: (Int, Int) -> Char
    cellChar pos
      | Just whitePos <- white, pos == whitePos = 'W'
      | Just blackPos <- black, pos == blackPos = 'B'
      | otherwise = '_'
