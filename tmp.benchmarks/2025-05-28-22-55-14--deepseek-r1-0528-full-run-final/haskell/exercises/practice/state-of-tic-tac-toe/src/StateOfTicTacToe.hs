module StateOfTicTacToe (gameState, GameState(..)) where

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

gameState :: [String] -> GameState
gameState board = 
    let countX = length [ () | row <- board, cell <- row, cell == 'X' ]
        countO = length [ () | row <- board, cell <- row, cell == 'O' ]
    in if countX < countO || countX > countO + 1
        then Impossible
        else 
            let rows = board
                cols = [ [ board !! i !! j | i <- [0..2] ] | j <- [0..2] ]
                diag1 = [board !! 0 !! 0, board !! 1 !! 1, board !! 2 !! 2]
                diag2 = [board !! 0 !! 2, board !! 1 !! 1, board !! 2 !! 0]
                allLines = rows ++ cols ++ [diag1, diag2]
                winX = any (all (=='X')) allLines
                winO = any (all (=='O')) allLines
            in if winX && winO
                then Impossible
                else if winX
                    then if countX == countO + 1 then WinX else Impossible
                    else if winO
                        then if countX == countO then WinO else Impossible
                        else if countX + countO == 9
                            then Draw
                            else Ongoing
