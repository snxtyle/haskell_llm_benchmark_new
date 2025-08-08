module StateOfTicTacToe (gameState, GameState(..)) where

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

gameState :: [String] -> GameState
gameState board = 
    let flatBoard = concat board
        xCount = length $ filter (== 'X') flatBoard
        oCount = length $ filter (== 'O') flatBoard
        xWins = hasWon 'X' board
        oWins = hasWon 'O' board
        boardFull = all (/= ' ') flatBoard
    in
        -- Check for invalid states
        if xCount < oCount || xCount > oCount + 1 then
            Impossible  -- Wrong turn order (X always goes first)
        else if xWins && oWins then
            Impossible  -- Both players can't win
        else if xWins && xCount == oCount then
            Impossible  -- X won but O played after
        else if oWins && xCount > oCount then
            Impossible  -- O won but X played after
        -- Check for valid end states
        else if xWins then
            WinX
        else if oWins then
            WinO
        else if boardFull then
            Draw
        else
            Ongoing

-- Check if a player has won
hasWon :: Char -> [String] -> Bool
hasWon player board = 
    checkRows || checkCols || checkDiagonals
  where
    checkRows = any (all (== player)) board
    
    checkCols = any (\col -> all (\row -> row !! col == player) board) [0..2]
    
    checkDiagonals = 
        all (\i -> (board !! i) !! i == player) [0..2] ||  -- Main diagonal
        all (\i -> (board !! i) !! (2 - i) == player) [0..2]  -- Anti-diagonal
