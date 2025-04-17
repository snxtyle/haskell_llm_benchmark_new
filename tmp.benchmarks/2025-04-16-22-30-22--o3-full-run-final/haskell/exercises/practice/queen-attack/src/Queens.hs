module Queens (boardString, canAttack) where

import Data.List (intercalate)

type Position = (Int, Int)

-------------------------------------------------------------------------------
-- Public API
-------------------------------------------------------------------------------

-- | Render the chess board as a string.
--
--   The board is always 8×8. Each square is represented by a single
--   character and squares are separated by a single space.
--     *  '_' for an empty square
--     *  'W' for the white queen
--     *  'B' for the black queen
--
--   Rows are joined by newline characters.  The first row corresponds to the
--   top of the board (rank 8 in chess notation).
--
--   If both queens are supplied they must occupy different squares; otherwise
--   an error is raised.  Any coordinates outside the 0‑7 range are also
--   considered invalid and will result in an error.
boardString :: Maybe Position -> Maybe Position -> String
boardString white black =
    validateBoard white black `seq` renderBoard
  where
    renderBoard = intercalate "\n" [renderRow r | r <- [0 .. 7]]

    renderRow r = unwords [renderSquare (r, c) | c <- [0 .. 7]]

    renderSquare pos
      | Just pos == white = "W"
      | Just pos == black = "B"
      | otherwise         = "_"

-- | Determine whether two queens can attack one another.
--
--   A queen can attack along rows, columns, or any diagonal.
--   The function raises an error if any of the supplied positions are outside
--   the board or if both queens occupy the same square.
canAttack :: Position -> Position -> Bool
canAttack queenA queenB =
    validatePosition queenA `seq`
    validatePosition queenB `seq`
    ensureDifferent queenA queenB `seq`
    sameRow || sameColumn || sameDiagonal
  where
    (ra, ca) = queenA
    (rb, cb) = queenB

    sameRow       = ra == rb
    sameColumn    = ca == cb
    sameDiagonal  = abs (ra - rb) == abs (ca - cb)

-------------------------------------------------------------------------------
-- Validation helpers
-------------------------------------------------------------------------------

validateBoard :: Maybe Position -> Maybe Position -> ()
validateBoard w b =
    maybeValidate w `seq`
    maybeValidate b `seq`
    case (w, b) of
      (Just p, Just q) -> ensureDifferent p q
      _                -> ()
  where
    maybeValidate = maybe () validatePosition

validatePosition :: Position -> ()
validatePosition (r, c)
  | inRange r && inRange c = ()
  | otherwise              = error "Invalid board position"
  where
    inRange x = 0 <= x && x <= 7

ensureDifferent :: Position -> Position -> ()
ensureDifferent p q
  | p == q    = error "Queens cannot occupy the same position"
  | otherwise = ()
