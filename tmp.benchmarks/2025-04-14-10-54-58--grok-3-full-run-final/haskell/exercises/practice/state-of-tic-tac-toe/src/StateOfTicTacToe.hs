module StateOfTicTacToe (gameState, GameState(..)) where

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

gameState :: [String] -> GameState
gameState board = 
    let 
        -- Count X's and O's
        xCount = length $ filter (== 'X') $ concat board
        oCount = length $ filter (== 'O') $ concat board
        
        -- Check if a player has won
        hasWon player = any (all (== player)) (rows ++ cols ++ diags)
            where
                rows = board
                cols = [[board !! i !! j | i <- [0..2]] | j <- [0..2]]
                diags = [[board !! i !! i | i <- [0..2]], [board !! i !! (2-i) | i <- [0..2]]]
        
        xWins = hasWon 'X'
        oWins = hasWon 'O'
        
        -- Check if board is full
        isFull = xCount + oCount == 9
        
        -- Validate turn order (X starts, players alternate)
        validTurns = abs (xCount - oCount) <= 1 && xCount >= oCount
        
        -- Check if game continued after a win
        -- A win requires at least 3 moves for X or O, so we check if moves exceed the minimum needed for a win
        continuedAfterWin = (xWins && xCount > 3 && xCount + oCount > 5) || (oWins && oCount > 3 && xCount + oCount > 6)
    in
        if not validTurns || continuedAfterWin || (xWins && oWins)
        then Impossible
        else if xWins
        then WinX
        else if oWins
        then WinO
        else if isFull
        then Draw
        else Ongoing
