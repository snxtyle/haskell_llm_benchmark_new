module StateOfTicTacToe
  ( gameState
  , GameState(..)
  ) where

import Data.List (transpose)

data GameState = WinX | WinO | Draw | Ongoing | Impossible
  deriving (Eq, Show)

-- | Determine the current game state of a Tic‑Tac‑Toe board.
--   The board is supplied as a list of strings that together must contain
--   exactly nine characters.  Each character must be either
--
--   • 'X' – an X played by the first player
--   • 'O' – an O played by the second player
--   • '_' or ' ' – an empty cell
--
--   Any deviation from these requirements makes the board 'Impossible'.
--
--   Otherwise the returned value is:
--
--   • 'WinX'      – X has a winning line.
--   • 'WinO'      – O has a winning line.
--   • 'Draw'      – the board is full and neither player has won.
--   • 'Ongoing'   – the game can still be played.
--   • 'Impossible'– the board is not reachable from an ordered sequence
--                   of valid moves starting with X.
gameState :: [String] -> GameState
gameState board
  | length cells /= 9                       = Impossible
  | not (all validChar cells)               = Impossible
  | oCount > xCount                         = Impossible
  | xCount - oCount > 1                     = Impossible
  | xWins && oWins                          = Impossible
  | xWins && xCount == oCount               = Impossible
  | oWins && xCount /= oCount               = Impossible
  | xWins                                   = WinX
  | oWins                                   = WinO
  | blankCount == 0                         = Draw
  | otherwise                               = Ongoing
  where
    cells       = concat board
    validChar c = c `elem` ['X','O','_',' ']
    xCount      = count 'X' cells
    oCount      = count 'O' cells
    blankCount  = length (filter isBlank cells)

    isBlank c = c == '_' || c == ' '

    rows   = chunksOf 3 cells
    cols   = transpose rows
    diags  = [[cells !! 0, cells !! 4, cells !! 8]
             , [cells !! 2, cells !! 4, cells !! 6]]

    linesOfBoard = rows ++ cols ++ diags

    winsBy ch = any (all (== ch)) linesOfBoard

    xWins = winsBy 'X'
    oWins = winsBy 'O'

    count ch = length . filter (== ch)

-- | Split a list into chunks of /n/ elements.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)
