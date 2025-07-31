module StateOfTicTacToe (gameState, GameState(..)) where

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

gameState :: [String] -> GameState
gameState board 
    | not (isValidBoard board) = Impossible
    | xWins && oWins = Impossible
    | xWins = WinX
    | oWins = WinO
    | isFull = Draw
    | otherwise = Ongoing
  where
    xWins = hasWin board 'X'
    oWins = hasWin board 'O'
    isFull = all (\c -> c == 'X' || c == 'O') (concat board)

isValidBoard :: [String] -> Bool
isValidBoard board = 
    let flatBoard = concat board
        xCount = length (filter (== 'X') flatBoard)
        oCount = length (filter (== 'O') flatBoard)
        xWins = hasWin board 'X'
        oWins = hasWin board 'O'
    in
        -- Check board dimensions
        length board == 3 && all ((== 3) . length) board &&
        -- Check valid characters
        all (\c -> c == 'X' || c == 'O' || c == ' ') flatBoard &&
        -- Check turn order
        (xCount == oCount || xCount == oCount + 1) &&
        -- Check game continuation after win
        not (xWins && oWins) &&
        not (xWins && xCount == oCount) &&  -- X wins but O moved last (impossible)
        not (oWins && xCount > oCount)      -- O wins but X moved last (impossible)

hasWin :: [String] -> Char -> Bool
hasWin board player = 
    -- Check rows
    any (all (== player)) board ||
    -- Check columns
    any (all (== player)) (transpose board) ||
    -- Check diagonals
    all (== player) [board !! i !! i | i <- [0..2]] ||
    all (== player) [board !! i !! (2-i) | i <- [0..2]]

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:_) = []
transpose xs = map head xs : transpose (map tail xs)
