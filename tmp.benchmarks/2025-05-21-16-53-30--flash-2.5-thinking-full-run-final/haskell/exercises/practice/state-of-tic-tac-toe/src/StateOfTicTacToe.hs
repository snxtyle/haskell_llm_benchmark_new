module StateOfTicTacToe (gameState, GameState(..)) where

import Data.List (transpose) -- For checking columns

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

-- Helper to count occurrences of a specific character on the board.
countChar :: Char -> [[Char]] -> Int
countChar c board = sum $ map (length . filter (== c)) board

-- Helper to check if a given player has achieved a winning line (row, column, or diagonal).
checkWin :: Char -> [[Char]] -> Bool
checkWin player board =
    -- Check rows
    any (all (== player)) board ||
    -- Check columns by transposing the board and checking rows
    any (all (== player)) (transpose board) ||
    -- Check main diagonal (top-left to bottom-right)
    all (== player) [board !! i !! i | i <- [0..2]] ||
    -- Check anti-diagonal (top-right to bottom-left)
    all (== player) [board !! i !! (2 - i) | i <- [0..2]]

gameState :: [String] -> GameState
gameState boardStrings =
    let
        -- Clean the board strings to extract only 'X', 'O', or ' ' characters.
        -- This handles the input format (e.g., " X | O | X ") by filtering out non-game characters.
        -- The resulting 'board' is a 3x3 list of characters.
        board = map (filter (`elem` "XO ")) boardStrings
        numX = countChar 'X' board
        numO = countChar 'O' board
        winX = checkWin 'X' board
        winO = checkWin 'O' board
        isFull = numX + numO == 9
    in
    -- 1. Check for impossible states based on counts (turn order).
    --    'X' always starts. So, the number of 'X's must be equal to 'O's or one more than 'O's.
    --    If 'O' has more marks than 'X', or 'X' has two or more marks than 'O', it's impossible.
    if numO > numX || numX > numO + 1
    then Impossible
    -- 2. Check for impossible states based on simultaneous wins.
    --    A game ends as soon as one player wins, so both cannot have winning lines simultaneously.
    else if winX && winO
    then Impossible
    -- 3. Check for impossible states based on win/turn count mismatch.
    --    If 'X' won, 'X' must have made the last move, so numX must be numO + 1.
    --    If 'O' won, 'O' must have made the last move, so numX must be numO.
    --    Any deviation means the game continued after a win, which is impossible.
    else if (winX && numX == numO) || (winO && numX == numO + 1)
    then Impossible
    -- 4. Determine actual game state based on valid conditions.
    else if winX
    then WinX
    else if winO
    then WinO
    else if isFull
    then Draw
    else Ongoing
