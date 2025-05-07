module Queens (boardString, canAttack) where

import Data.List (intercalate)

-- | Produce a textual representation of the board.
--   The board is always 8×8.  Empty squares are rendered as “_”.
--   A white queen is rendered as “W”, a black queen as “B”.
--   Squares are separated by a single space, and rows are separated
--   by a newline.  The first row corresponds to rank 0 (the “top”
--   of the board), the last row to rank 7.  The resulting string
--   MUST end with a trailing newline (see test-suite expectations).
boardString :: Maybe (Int, Int)  -- ^ White queen position
            -> Maybe (Int, Int)  -- ^ Black queen position
            -> String
boardString white black =
    intercalate "\n" [rowStr r | r <- [0 .. 7]] ++ "\n"
  where
    rowStr r = intercalate " " [square r c | c <- [0 .. 7]]
    square r c
        | Just (r, c) == white = "W"
        | Just (r, c) == black = "B"
        | otherwise            = "_"

-- | Determine whether two queens can attack each other.
--   They can attack if they share the same row, column,
--   or are on the same diagonal.
canAttack :: (Int, Int)  -- ^ First queen (row, column)
          -> (Int, Int)  -- ^ Second queen (row, column)
          -> Bool
canAttack (r1, c1) (r2, c2) =
       r1 == r2                         -- same row
    || c1 == c2                         -- same column
    || abs (r1 - r2) == abs (c1 - c2)   -- same diagonal
