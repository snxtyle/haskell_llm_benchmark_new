module StateOfTicTacToe (gameState, GameState(..)) where

import Data.List (transpose)

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

-- | Determine the current state of a tic-tac-toe board.
--
--   The board must be supplied as a list of exactly three strings, each of
--   length three.  The characters are interpreted as follows:
--
--   * 'X' – a square occupied by player X
--   * 'O' – a square occupied by player O
--   * ' ' – an empty square (an underscore '_' is also accepted for convenience)
--
--   If the board is not reachable by taking turns starting with X, or if the
--   game continued after somebody had already won, the result is 'Impossible'.
gameState :: [String] -> GameState
gameState board
  | not (isValidShape board)           = Impossible
  | not (all validChar flat)           = Impossible
  | xCount < oCount                    = Impossible
  | xCount > oCount + 1                = Impossible
  | xWins && oWins                     = Impossible
  | xWins && xCount /= oCount + 1      = Impossible
  | oWins && xCount /= oCount          = Impossible
  | xWins                              = WinX
  | oWins                              = WinO
  | full                               = Draw
  | otherwise                          = Ongoing
  where
    normalizedBoard = map (map normalizeChar) board
    flat            = concat normalizedBoard
    xCount          = count 'X' flat
    oCount          = count 'O' flat
    full            = count ' ' flat == 0
    xWins           = hasWinner 'X' normalizedBoard
    oWins           = hasWinner 'O' normalizedBoard

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

isValidShape :: [String] -> Bool
isValidShape b = length b == 3 && all ((== 3) . length) b

-- Accept both space and underscore as the “empty” marker.
normalizeChar :: Char -> Char
normalizeChar c
  | c == '_'   = ' '
  | otherwise  = c

validChar :: Char -> Bool
validChar c = c `elem` ['X', 'O', ' ']

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

hasWinner :: Char -> [String] -> Bool
hasWinner sym b = any (all (== sym)) (rows ++ cols ++ dias)
  where
    rows = b
    cols = transpose b
    dias = [ [b !! 0 !! 0, b !! 1 !! 1, b !! 2 !! 2]
           , [b !! 0 !! 2, b !! 1 !! 1, b !! 2 !! 0]
           ]
