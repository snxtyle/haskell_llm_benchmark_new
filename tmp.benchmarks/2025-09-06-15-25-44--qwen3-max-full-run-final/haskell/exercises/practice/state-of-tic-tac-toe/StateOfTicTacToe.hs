module StateOfTicTacToe (gameState, GameState(..)) where

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

gameState :: [String] -> GameState
gameState board
  | not (isValidBoard board) = Impossible
  | hasWon 'X' board = WinX
  | hasWon 'O' board = WinO
  | isFull board = Draw
  | otherwise = Ongoing

-- Check if the board is valid (correct turn order and no playing after game end)
isValidBoard :: [String] -> Bool
isValidBoard board = 
  let xCount = countPieces 'X' board
      oCount = countPieces 'O' board
      xWins = hasWon 'X' board
      oWins = hasWon 'O' board
  in 
    -- X starts, so X count should be equal to or one more than O count
    (xCount == oCount || xCount == oCount + 1) &&
    -- Can't have both X and O winning
    not (xWins && oWins) &&
    -- If X won, X should have one more piece than O
    not (xWins && xCount /= oCount + 1) &&
    -- If O won, counts should be equal
    not (oWins && xCount /= oCount) &&
    -- Board dimensions should be 3x3
    length board == 3 && all (\row -> length row == 3) board

-- Count occurrences of a piece on the board
countPieces :: Char -> [String] -> Int
countPieces piece board = sum [length (filter (== piece) row) | row <- board]

-- Check if a player has won
hasWon :: Char -> [String] -> Bool
hasWon player board =
  -- Check rows
  any (all (== player)) board ||
  -- Check columns
  any (all (== player)) (transpose board) ||
  -- Check diagonals
  all (== player) [board !! 0 !! 0, board !! 1 !! 1, board !! 2 !! 2] ||
  all (== player) [board !! 0 !! 2, board !! 1 !! 1, board !! 2 !! 0]

-- Transpose a 2D list (rows to columns)
transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([] : xss) = transpose xss
transpose ((x:xs) : xss) = (x : [h | (h:_) <- xss]) : transpose (xs : [t | (_:t) <- xss])

-- Check if the board is full
isFull :: [String] -> Bool
isFull board = not (any (elem ' ') board)
