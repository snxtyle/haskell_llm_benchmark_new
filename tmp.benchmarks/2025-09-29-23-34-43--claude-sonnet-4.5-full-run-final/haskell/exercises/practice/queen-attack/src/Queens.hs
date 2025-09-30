module Queens (boardString, canAttack) where

import Data.List (intercalate)

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = unlines rows
  where
    rows = [rowString r | r <- [0..7]]
    rowString r = intercalate " " [cellChar r c | c <- [0..7]]
    cellChar r c
      | Just (r, c) == white = "W"
      | Just (r, c) == black = "B"
      | otherwise = "_"

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (r1, c1) (r2, c2) = sameRow || sameColumn || sameDiagonal
  where
    sameRow = r1 == r2
    sameColumn = c1 == c2
    sameDiagonal = abs (r1 - r2) == abs (c1 - c2)
