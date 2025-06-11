module StateOfTicTacToe (gameState, GameState(..)) where

import Data.List (transpose)

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

gameState :: [String] -> GameState
gameState board
    | numX < numO || numX > numO + 1 = Impossible
    | xWins && oWins                 = Impossible
    | xWins && numX == numO          = Impossible
    | oWins && numX > numO           = Impossible
    | xWins                          = WinX
    | oWins                          = WinO
    | numX + numO == 9               = Draw
    | otherwise                      = Ongoing
  where
    flatBoard = concat board
    numX = count 'X'
    numO = count 'O'
    xWins = checkWin 'X'
    oWins = checkWin 'O'

    count player = length $ filter (== player) flatBoard

    checkWin :: Char -> Bool
    checkWin player =
        let
            allPlayer = all (== player)
            rows = board
            cols = transpose board
            diag1 = [(board !! i) !! i | i <- [0..2]]
            diag2 = [(board !! i) !! (2-i) | i <- [0..2]]
        in
            any allPlayer rows ||
            any allPlayer cols ||
            allPlayer diag1 ||
            allPlayer diag2
