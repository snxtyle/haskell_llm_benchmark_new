module Connect (Mark(..), winner) where

-- | The two possible players.
data Mark = Cross | Nought deriving (Eq, Show)

-- | Determine the winner on a Hex / Polygon board.
--
-- The board is passed in as a list of strings where the characters
-- '.', 'X', 'O' represent an empty field, a Cross‑stone and a
-- Nought‑stone respectively.  Any blanks that are present only serve
-- for pretty printing and are ignored.
--
-- The player using Crosses tries to connect the left edge with the
-- right edge.
-- The player using Noughts tries to connect the top edge with the
-- bottom edge.
--
-- The function returns:
--   * Just Cross   – if Cross has a connecting path and Nought does not
--   * Just Nought  – if Nought has a connecting path and Cross does not
--   * Nothing      – if nobody (or both – which should be impossible
--                    in a valid Hex position) has a connecting path
winner :: [String] -> Maybe Mark
winner rawBoard =
  case (crossWins, noughtWins) of
    (True, False)  -> Just Cross
    (False, True)  -> Just Nought
    _              -> Nothing
  where
    board       = parseBoard rawBoard
    crossWins   = hasWinningPath board 'X'
    noughtWins  = hasWinningPath board 'O'

--------------------------------------------------------------------
-- Internal helpers
--------------------------------------------------------------------

type Board = [String]           -- each string is a row without blanks
type Coord = (Int, Int)         -- (row, column)

-- Remove every space character – indentation and separators – so that
-- the remaining strings contain only the actual fields.
parseBoard :: [String] -> Board
parseBoard = map (filter (/= ' '))

--------------------------------------------------------------------
-- Path search
--------------------------------------------------------------------

-- | Check whether the given player character has a winning path.
hasWinningPath :: Board -> Char -> Bool
hasWinningPath board player
  | null board = False                -- empty board – nobody wins
  | otherwise  = dfs startCoords []
  where
    height = length board

    -- Collect start coordinates situated on the player’s starting edge.
    startCoords :: [Coord]
    startCoords = case player of
      'X' -> [ (r, 0)
             | r <- [0 .. height - 1]
             , cell (r, 0) == player
             ]
      'O' -> [ (0, c)
             | c <- [0 .. widthTop - 1]
             , cell (0, c) == player
             ]
      _   -> []

    widthTop :: Int
    widthTop = if null board then 0 else length (head board)

    -- Fetch the content of a cell (assumes indices are in range).
    cell :: Coord -> Char
    cell (r, c) = board !! r !! c

    -- Depth‑first search keeping track of already visited nodes.
    dfs :: [Coord] -> [Coord] -> Bool
    dfs [] _ = False
    dfs (p:ps) visited
      | p `elem` visited = dfs ps visited
      | onGoalEdge p     = True
      | otherwise        =
          let nxt = filter isPlayer (neighbours board p)
          in dfs (nxt ++ ps) (p : visited)

    isPlayer :: Coord -> Bool
    isPlayer coord = cell coord == player

    -- Determine whether the current coordinate is on the target edge.
    onGoalEdge :: Coord -> Bool
    onGoalEdge (r, c) = case player of
      'X' -> c == (length (board !! r) - 1)
      'O' -> r == height - 1
      _   -> False

--------------------------------------------------------------------
-- Hex‑grid neighbourhood
--------------------------------------------------------------------

-- | All neighbour coordinates on a hex grid represented as a
-- parallel‑off‑set rectangular array.
neighbours :: Board -> Coord -> [Coord]
neighbours board (r, c) = filter inBounds candidates
  where
    candidates =
      [ (r - 1, c)     -- up‑left
      , (r - 1, c + 1) -- up‑right
      , (r,     c - 1) -- left
      , (r,     c + 1) -- right
      , (r + 1, c - 1) -- down‑left
      , (r + 1, c)     -- down‑right
      ]

    inBounds (ri, ci) =
         ri >= 0
      && ri < length board
      && ci >= 0
      && ci < length (board !! ri)
