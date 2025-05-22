module StateOfTicTacToe (gameState, GameState(..)) where

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

gameState :: [String] -> GameState
gameState board
  | not (isValidBoard board) = Impossible
  | hasWinner 'X' board && hasWinner 'O' board = Impossible
  | hasWinner 'X' board = if isValidAfterWin 'X' board then WinX else Impossible
  | hasWinner 'O' board = if isValidAfterWin 'O' board then WinO else Impossible
  | isBoardFull board = Draw
  | otherwise = Ongoing

-- Check if the board has valid turn order
isValidBoard :: [String] -> Bool
isValidBoard board = 
  let xCount = countChar 'X' board
      oCount = countChar 'O' board
  in xCount >= oCount && xCount <= oCount + 1

-- Count occurrences of a character in the board
countChar :: Char -> [String] -> Int
countChar c board = length $ filter (== c) $ concat board

-- Check if a player has won
hasWinner :: Char -> [String] -> Bool
hasWinner player board = 
  let rows = board
      cols = [[board !! i !! j | i <- [0..2]] | j <- [0..2]]
      diag1 = [board !! i !! i | i <- [0..2]]
      diag2 = [board !! i !! (2-i) | i <- [0..2]]
      lines = rows ++ cols ++ [diag1, diag2]
  in any (all (== player)) lines

-- Check if the board is full
isBoardFull :: [String] -> Bool
isBoardFull board = all (/= ' ') $ concat board

-- Check if the game state is valid after a win
isValidAfterWin :: Char -> [String] -> Bool
isValidAfterWin winner board =
  let xCount = countChar 'X' board
      oCount = countChar 'O' board
  in if winner == 'X'
     then xCount == oCount + 1  -- X wins, so X should have one more piece
     else xCount == oCount      -- O wins, so counts should be equal
