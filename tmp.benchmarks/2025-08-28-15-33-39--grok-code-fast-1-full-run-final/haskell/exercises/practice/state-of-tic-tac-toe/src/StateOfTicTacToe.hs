module StateOfTicTacToe (gameState, GameState(..)) where

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

gameState :: [String] -> GameState
gameState board =
  if length board /= 3 || any (\row -> length row /= 3) board
    then Impossible
    else
      let flat = concat board
          validChars = all (`elem` "XO ") flat
      in if not validChars
        then Impossible
        else
          let countX = length $ filter (== 'X') flat
              countO = length $ filter (== 'O') flat
              emptyCount = length $ filter (== ' ') flat
          in if countO > countX || countX > countO + 1
            then Impossible
            else
              let hasWinX = checkWin 'X' board
                  hasWinO = checkWin 'O' board
              in if hasWinX && hasWinO
                then Impossible
                else if hasWinX
                  then if countX /= countO + 1 then Impossible else WinX
                  else if hasWinO
                    then if countX /= countO then Impossible else WinO
                    else if emptyCount == 0 then Draw
                    else Ongoing

checkWin :: Char -> [String] -> Bool
checkWin player board =
  let rows = map (all (== player)) board
      cols = map (all (== player)) (transpose board)
      diag1 = all (== player) [board !! i !! i | i <- [0..2]]
      diag2 = all (== player) [board !! i !! (2 - i) | i <- [0..2]]
  in or (rows ++ cols ++ [diag1, diag2])

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([] : _) = []
transpose xs = map head xs : transpose (map tail xs)
