module Queens (boardString, canAttack) where

import Data.List (intercalate)

-- | Generates a string representation of the board with queens placed.
-- Arguments are Maybe (row, col) for white and black queens.
boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black =
  unlines [ unwords [ cellChar r c | c <- [0..7] ] | r <- [0..7] ]
  where
    cellChar :: Int -> Int -> String
    cellChar r c
      | Just (r, c) == white = "W"
      | Just (r, c) == black = "B"
      | otherwise            = "_"

-- | Determines if two queens can attack each other.
-- Arguments are (row, col) tuples for each queen.
canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (r1, c1) (r2, c2)
  | r1 < 0 || r1 > 7 || c1 < 0 || c1 > 7 = error "Invalid position for queen A"
  | r2 < 0 || r2 > 7 || c2 < 0 || c2 > 7 = error "Invalid position for queen B"
  | r1 == r2 && c1 == c2 = error "Queens cannot occupy the same square" -- Or return False, depending on interpretation. Let's error for invalid state.
  | r1 == r2         = True  -- Same row
  | c1 == c2         = True  -- Same column
  | abs (r1 - r2) == abs (c1 - c2) = True  -- Same diagonal
  | otherwise        = False
