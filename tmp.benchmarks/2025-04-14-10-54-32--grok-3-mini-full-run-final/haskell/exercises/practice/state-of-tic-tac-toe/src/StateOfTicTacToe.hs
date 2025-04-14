module StateOfTicTacToe (gameState, GameState(..)) where

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

gameState :: [String] -> GameState
gameState board
  | length board /= 9 = Impossible  -- Board must have exactly 9 elements
  | not (all (`elem` ["X", "O", " "]) board) = Impossible  -- Invalid characters
  | otherwise = 
      let numX = length $ filter (== "X") board
          numO = length $ filter (== "O") board
          xWon = hasWon "X" board
          oWon = hasWon "O" board
      in if numX /= numO && numX /= numO + 1
         then Impossible  -- Invalid turn order
         else if xWon && oWon
              then Impossible  -- Both cannot win
              else if xWon && numX == numO + 1 && not oWon
                   then WinX
              else if oWon && numX == numO && not xWon
                   then WinO
              else if not xWon && not oWon && all (/= " ") board  -- Board is full
                   then Draw
              else if not xWon && not oWon
                   then Ongoing
              else Impossible  -- Other invalid cases

-- Helper function to check if a player has won
hasWon :: String -> [String] -> Bool
hasWon player board = any (\indices -> all (\i -> board !! i == player) indices) winningIndices
  where
    winningIndices = [ [0,1,2], [3,4,5], [6,7,8]  -- Rows
                     , [0,3,6], [1,4,7], [2,5,8]  -- Columns
                     , [0,4,8], [2,4,6]          -- Diagonals
                     ]
