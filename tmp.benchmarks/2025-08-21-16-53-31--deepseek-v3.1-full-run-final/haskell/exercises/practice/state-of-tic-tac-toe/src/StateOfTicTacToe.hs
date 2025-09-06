module StateOfTicTacToe (gameState, GameState(..)) where

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

gameState :: [String] -> GameState
gameState board
  | not (isValidBoard board) = Impossible
  | xWins && oWins = Impossible
  | xWins = WinX
  | oWins = WinO
  | fullBoard = Draw
  | otherwise = Ongoing
  where
    xWins = hasWon 'X' board
    oWins = hasWon 'O' board
    fullBoard = all (not . elem ' ') board

-- Check if the board is valid
isValidBoard :: [String] -> Bool
isValidBoard board = 
  let xCount = countChar 'X' board
      oCount = countChar 'O' board
      xWins = hasWon 'X' board
      oWins = hasWon 'O' board
  in
    -- X should have same or one more count than O
    (xCount == oCount || xCount == oCount + 1) &&
    -- Both can't win
    not (xWins && oWins) &&
    -- If X won, X must have made the last move (one more X than O)
    (if xWins then xCount == oCount + 1 else True) &&
    -- If O won, O must have made the last move (equal count)
    (if oWins then xCount == oCount else True)

-- Count occurrences of a character in the board
countChar :: Char -> [String] -> Int
countChar c board = sum $ map (length . filter (== c)) board

-- Check if a player has won
hasWon :: Char -> [String] -> Bool
hasWon player board =
  -- Check rows
  any (all (== player)) board ||
  -- Check columns
  any (all (== player)) (transpose board) ||
  -- Check diagonals
  (board !! 0 !! 0 == player && board !! 1 !! 1 == player && board !! 2 !! 2 == player) ||
  (board !! 0 !! 2 == player && board !! 1 !! 1 == player && board !! 2 !! 0 == player)

-- Transpose a list of strings (convert columns to rows)
transpose :: [String] -> [String]
transpose [r1, r2, r3] = 
  [[r1 !! 0, r2 !! 0, r3 !! 0],
   [r1 !! 1, r2 !! 1, r3 !! 1],
   [r1 !! 2, r2 !! 2, r3 !! 2]]
transpose _ = error "Board must have exactly 3 rows"
