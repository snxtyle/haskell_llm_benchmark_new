module StateOfTicTacToe (gameState, GameState(..)) where

import Data.List (transpose)

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

-- Helper function to check if a player has won
checkWin :: Char -> [String] -> Bool
checkWin player board = any winningLine (rows ++ cols ++ diags)
  where
    n = length board -- Assuming square board, usually 3
    rows = board
    cols = transpose board
    diag1 = [board !! i !! i | i <- [0..n-1]]
    diag2 = [board !! i !! (n-1-i) | i <- [0..n-1]]
    diags = [diag1, diag2]
    winningLine = all (== player)

-- Main function to determine the game state
gameState :: [String] -> GameState
gameState board
    -- Validate board dimensions and characters
    | length board /= 3 || any ((/= 3) . length) board = error "Invalid board: Must be 3x3."
    | not (all (\c -> c `elem` "XO ") flatBoard) = error "Invalid board: Contains invalid characters."
    -- Check game logic
    | nx < no || nx > no + 1 = Impossible  -- Invalid number of Xs vs Os
    | winX && winO           = Impossible  -- Both cannot win simultaneously
    | winX && nx == no       = Impossible  -- X wins, but O appears to have made the last move
    | winO && nx > no        = Impossible  -- O wins, but X appears to have made the last move
    | winX                   = WinX        -- X wins
    | winO                   = WinO        -- O wins
    | nx + no == 9           = Draw        -- Board is full, no winner
    | otherwise              = Ongoing     -- Game is still in progress
  where
    flatBoard = concat board
    nx = length $ filter (== 'X') flatBoard
    no = length $ filter (== 'O') flatBoard
    winX = checkWin 'X' board
    winO = checkWin 'O' board

