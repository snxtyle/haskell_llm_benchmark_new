module StateOfTicTacToe (gameState, GameState(..)) where

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

gameState :: [String] -> GameState
gameState board
  | not (validBoard board) = Impossible
  | not (validTurnOrder board) = Impossible
  | winX && winO = Impossible
  | winX && countX == countO = Impossible  -- X won but O played after
  | winO && countX > countO = Impossible   -- O won but X played after
  | winX = WinX
  | winO = WinO
  | isFull board = Draw
  | otherwise = Ongoing
  where
    flatBoard = concat board
    countX = length $ filter (== 'X') flatBoard
    countO = length $ filter (== 'O') flatBoard
    winX = hasWon 'X' board
    winO = hasWon 'O' board

-- Check if the board has valid dimensions and characters
validBoard :: [String] -> Bool
validBoard board = length board == 3 && all (\row -> length row == 3 && all (`elem` "XO ") row) board

-- Check if turn order is valid (X starts, players alternate)
validTurnOrder :: [String] -> Bool
validTurnOrder board = countX == countO || countX == countO + 1
  where
    flatBoard = concat board
    countX = length $ filter (== 'X') flatBoard
    countO = length $ filter (== 'O') flatBoard

-- Check if the board is full
isFull :: [String] -> Bool
isFull board = ' ' `notElem` concat board

-- Check if a player has won
hasWon :: Char -> [String] -> Bool
hasWon player board = any (all (== player)) allLines
  where
    -- Get all rows, columns, and diagonals
    allLines = rows ++ columns ++ diagonals
    rows = board
    columns = [[board !! r !! c | r <- [0..2]] | c <- [0..2]]
    diagonals = [[board !! i !! i | i <- [0..2]], 
                 [board !! i !! (2-i) | i <- [0..2]]]
