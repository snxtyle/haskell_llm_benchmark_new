module StateOfTicTacToe (gameState, GameState(..)) where

import Data.List (transpose)

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

-- | Determine the state of a tic‑tac‑toe game.
--
-- The board is expected to be a list of three strings, each of length three,
-- containing only the characters  'X', 'O', or a blank space ' '.
--
-- *  If the board is reachable following the rules, the result is
--    •  WinX   – X has a winning line
--    •  WinO   – O has a winning line
--    •  Draw   – the board is full and nobody has won
--    •  Ongoing – the game can still continue
--
-- *  If the board is not reachable by taking turns starting with X, or play
--    continued after the game had already been decided, the result is
--    Impossible.
gameState :: [String] -> GameState
gameState board
  | not (validStructure board) = Impossible
  | diffX_O < 0 || diffX_O > 1 = Impossible
  | xWins && oWins             = Impossible
  | xWins && diffX_O /= 1      = Impossible
  | oWins && diffX_O /= 0      = Impossible
  | xWins                      = WinX
  | oWins                      = WinO
  | fullBoard                  = Draw
  | otherwise                  = Ongoing
  where
    -- Counts of each mark
    countX  = length [c | row <- board, c <- row, c == 'X']
    countO  = length [c | row <- board, c <- row, c == 'O']
    diffX_O = countX - countO

    -- All possible winning lines (rows, columns, diagonals)
    linesToCheck = board ++ transpose board ++ diagonals board

    xWins = any (all (== 'X')) linesToCheck
    oWins = any (all (== 'O')) linesToCheck

    fullBoard = all (/= ' ') [c | row <- board, c <- row]

-- | Validate the shape and characters of the board.
validStructure :: [String] -> Bool
validStructure b =
  length b == 3 &&
  all (\row -> length row == 3) b &&
  all (`elem` allowed) (concat b)
  where
    allowed = "XO "

-- | Produce the two diagonal lines of the board.
diagonals :: [String] -> [String]
diagonals b =
  [ [ b !! i !! i     | i <- [0..2] ]
  , [ b !! i !! (2-i) | i <- [0..2] ]
  ]
