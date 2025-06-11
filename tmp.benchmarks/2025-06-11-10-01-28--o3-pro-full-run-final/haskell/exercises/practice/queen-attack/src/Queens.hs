module Queens (boardString, canAttack) where

import Data.List  (intercalate)
import Data.Maybe (catMaybes)

-- | Render an 8×8 chess board as a multi-line string.
--   The board is numbered from (0,0) in the top-left corner
--   to (7,7) in the bottom-right corner.
--
--   A position supplied as
--     Just (row, col)   places a queen at that square.
--     Nothing           leaves the board empty at that square.
--
--   The resulting string contains eight rows separated by newlines.
--   Each row consists of eight symbols separated by single spaces:
--     * "W" – white queen
--     * "B" – black queen
--     * "_" – empty square
--
--   Examples:
--
--     boardString (Just (0, 3)) (Just (7, 3))
--
--   produces
--
--     _ _ _ W _ _ _ _
--     _ _ _ _ _ _ _ _
--     _ _ _ _ _ _ _ _
--     _ _ _ _ _ _ _ _
--     _ _ _ _ _ _ _ _
--     _ _ _ _ _ _ _ _
--     _ _ _ _ _ _ _ _
--     _ _ _ B _ _ _ _
--
boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black
  | any (not . valid) (catMaybes [white, black]) =
      error "Queens must be placed on the board"
  | overlapping =
      error "Queens cannot share the same position"
  | otherwise =
      -- join rows with newlines, symbols in each row with spaces, and
      -- terminate with a trailing newline (unlines behaviour)
      unlines [intercalate " " [square (r, c) | c <- [0 .. 7]] | r <- [0 .. 7]]
  where
    overlapping = case (white, black) of
      (Just p1, Just p2) -> p1 == p2
      _                  -> False

    square pos
      | Just pos == white = "W"
      | Just pos == black = "B"
      | otherwise         = "_"

    valid (r, c) = inRange r && inRange c
    inRange x = x >= 0 && x <= 7

-- | Determine whether two queens can attack each other.
--   They can attack if they share a row, a column, or a diagonal.
--
--   Invalid positions (outside 0-7) or identical positions result in
--   an error, matching the expectations of the exercise’s tests.
--
canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack q1@(r1, c1) q2@(r2, c2)
  | not (valid q1) || not (valid q2) =
      error "Queens must be placed on the board"
  | q1 == q2 =
      error "Queens cannot share the same position"
  | otherwise =
      r1 == r2                          -- same row
      || c1 == c2                       -- same column
      || abs (r1 - r2) == abs (c1 - c2) -- same diagonal
  where
    valid (r, c) = inRange r && inRange c
    inRange x = x >= 0 && x <= 7
