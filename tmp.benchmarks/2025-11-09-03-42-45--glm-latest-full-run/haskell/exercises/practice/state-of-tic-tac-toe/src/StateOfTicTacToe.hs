module StateOfTicTacToe (gameState, GameState(..)) where

import Data.List (transpose)

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

-- | Determines the state of a tic-tac-toe game.
gameState :: [String] -> GameState
gameState board =
    let flatBoard = concat board
        numX = length $ filter (== 'X') flatBoard
        numO = length $ filter (== 'O') flatBoard
        xWins = playerHasWon 'X' board
        oWins = playerHasWon 'O' board
        isFull = not $ any (== ' ') flatBoard
    in
        -- Invalid turn order
        if numO > numX || numX - numO > 1
        then Impossible
        -- Both players can't win
        else if xWins && oWins
        then Impossible
        -- If X wins, they must have one more mark than O
        else if xWins && numX /= numO + 1
        then Impossible
        -- If O wins, they must have the same number of marks as X
        else if oWins && numX /= numO
        then Impossible
        -- Valid game states
        else if xWins
        then WinX
        else if oWins
        then WinO
        else if isFull
        then Draw
        else Ongoing

-- | Checks if a given player has won the game.
playerHasWon :: Char -> [String] -> Bool
playerHasWon player board = any (== replicate 3 player) lines
    where
        rows = board
        cols = transpose board
        diags = [[board!!0!!0, board!!1!!1, board!!2!!2],
                 [board!!0!!2, board!!1!!1, board!!2!!0]]
        lines = rows ++ cols ++ diags
