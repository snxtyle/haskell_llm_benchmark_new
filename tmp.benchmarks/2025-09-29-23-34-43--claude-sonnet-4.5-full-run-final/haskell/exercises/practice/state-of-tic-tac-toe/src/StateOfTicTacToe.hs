module StateOfTicTacToe (gameState, GameState(..)) where

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

gameState :: [String] -> GameState
gameState board
  | not validTurnOrder = Impossible
  | xWins && oWins = Impossible
  | xWins && countO > countX - 1 = Impossible
  | oWins && countX > countO = Impossible
  | xWins = WinX
  | oWins = WinO
  | isFull = Draw
  | otherwise = Ongoing
  where
    cells = concat board
    countX = length $ filter (== 'X') cells
    countO = length $ filter (== 'O') cells
    validTurnOrder = countX == countO || countX == countO + 1
    
    xWins = hasWin 'X'
    oWins = hasWin 'O'
    isFull = all (\c -> c == 'X' || c == 'O') cells
    
    hasWin player = any (all (== player)) allLines
    
    allLines = rows ++ cols ++ diags
    
    rows = board
    
    cols = [[board !! r !! c | r <- [0..2]] | c <- [0..2]]
    
    diags = [
        [board !! 0 !! 0, board !! 1 !! 1, board !! 2 !! 2],
        [board !! 0 !! 2, board !! 1 !! 1, board !! 2 !! 0]
      ]
