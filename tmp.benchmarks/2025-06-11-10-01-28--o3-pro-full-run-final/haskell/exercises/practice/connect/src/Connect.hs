module Connect (Mark(..), winner) where

import qualified Data.Set as Set

-- | A player's mark.
data Mark = Cross   -- ^ Player using 'X', tries to connect left-to-right.
          | Nought  -- ^ Player using 'O', tries to connect top-to-bottom.
          deriving (Eq, Show)

----------------------------------------------------------------
-- Public API
----------------------------------------------------------------

-- | Determine the winner of a Hex / Polygon game.
--   Returns:
--     * Just Cross   – player 'X' connects the left and right edges
--     * Just Nought  – player 'O' connects the top and bottom edges
--     * Nothing      – no player has yet completed a connection
winner :: [String] -> Maybe Mark
winner rawRows =
  let board = parseBoard rawRows
  in case (hasWon board Cross, hasWon board Nought) of
       (True,  False) -> Just Cross
       (False, True ) -> Just Nought
       (False, False) -> Nothing
       -- A valid Hex board cannot have both players winning simultaneously,
       -- but we guard against ill-formed inputs.
       (True,  True ) -> error "Invalid board: both players have a winning path"

----------------------------------------------------------------
-- Internal helpers
----------------------------------------------------------------

type Board  = [[Char]]
type Coord  = (Int, Int)  -- ^ (row, column)

-- | Strip the leading indentation spaces that the ASCII-art board uses,
--   yielding a proper rectangular matrix of characters.
parseBoard :: [String] -> Board
parseBoard = map (filter (/= ' '))

-- | Dimensions of the board: (height, width)
dims :: Board -> (Int, Int)
dims []        = (0, 0)
dims b@(r : _) = (length b, length r)

-- | Does the specified player have a winning connection?
hasWon :: Board -> Mark -> Bool
hasWon board mark =
  any reachesGoal startCells
  where
    (h, w) = dims board
    symbol = case mark of
               Cross  -> 'X'
               Nought -> 'O'

    -- Starting edge cells for each player
    startCells =
      case mark of
        Cross  -> [ (r, 0) | r <- [0 .. h - 1], boardAt (r, 0) == Just symbol ]
        Nought -> [ (0, c) | c <- [0 .. w - 1], boardAt (0, c) == Just symbol ]

    -- Checks whether a coordinate lies on the target edge
    goal (r, c) =
      case mark of
        Cross  -> c == w - 1
        Nought -> r == h - 1

    -- Depth-first search along connected stones of the same colour
    reachesGoal start = dfs Set.empty [start]
      where
        dfs _        []     = False
        dfs visited (p:ps)
          | p `Set.member` visited   = dfs visited ps
          | boardAt p /= Just symbol = dfs visited ps
          | goal p                   = True
          | otherwise =
              let visited'  = Set.insert p visited
                  nextCells = filter inBounds (neighbors p)
                  queue     = nextCells ++ ps
              in dfs visited' queue

    -- Safe cell lookup
    boardAt (r, c)
      | inBounds (r, c) = Just (board !! r !! c)
      | otherwise       = Nothing

    inBounds (r, c) = r >= 0 && r < h && c >= 0 && c < w

-- | Hex-grid neighbour offsets for this ASCII representation.
neighbors :: Coord -> [Coord]
neighbors (r, c) =
  [ (r - 1, c)     -- north
  , (r - 1, c + 1) -- north-east
  , (r,     c - 1) -- west
  , (r,     c + 1) -- east
  , (r + 1, c - 1) -- south-west
  , (r + 1, c)     -- south
  ]
