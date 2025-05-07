module StateOfTicTacToe (gameState, GameState(..)) where

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

-- Helper function to count character occurrences in a list
countChar :: Char -> [Char] -> Int
countChar c = length . filter (== c)

-- Helper function to check if a player has won
-- allChars is guaranteed to have 9 elements by the time this is called.
checkWinInternal :: Char -> [Char] -> Bool
checkWinInternal player allChars =
    let charAt idx = allChars !! idx -- Access character at a specific index (0-8)
        -- Define all 8 winning line combinations by their indices
        winningLinesIndices = [
            [0,1,2], [3,4,5], [6,7,8], -- rows
            [0,3,6], [1,4,7], [2,5,8], -- cols
            [0,4,8], [2,4,6]           -- diagonals
          ]
    -- Check if any winning line consists entirely of the player's marks
    in any (all (\idx -> charAt idx == player)) winningLinesIndices

gameState :: [String] -> GameState
gameState boardRows
    -- Validate board structure: must be 3 rows
    | length boardRows /= 3 = error "Invalid board: Must be 3 rows."
    -- Validate board structure: each row must have 3 chars
    | any (\row -> length row /= 3) boardRows = error "Invalid board: Each row must have 3 characters."
    | otherwise =
        let allChars = concat boardRows -- Flatten the board into a single list of characters
            nx = countChar 'X' allChars -- Count of 'X' marks
            no = countChar 'O' allChars -- Count of 'O' marks
            numEmpty = countChar ' ' allChars -- Count of empty spaces
        in
            -- Validate characters: total cells must be 9, and only 'X', 'O', or ' ' allowed.
            if nx + no + numEmpty /= 9 then
                error "Invalid board: Contains characters other than 'X', 'O', ' '."
            else
                -- Basic turn order validation: X starts.
                -- Number of X's must be equal to number of O's or one more.
                -- If no > nx (O played more than X) or nx > no + 1 (X played too many more than O), it's impossible.
                if no > nx || nx > no + 1 then
                    Impossible
                else
                    let xWins = checkWinInternal 'X' allChars -- Check if X has a winning line
                        oWins = checkWinInternal 'O' allChars -- Check if O has a winning line
                    in
                        if xWins && oWins then
                            -- If both players have winning lines, it's an impossible state.
                            -- This implies the game continued after one player had already won.
                            Impossible
                        else if xWins then
                            -- If X wins:
                            -- X must have made the last move, so nx must be no + 1.
                            -- If nx == no, it means O played after X won, or X won on O's turn.
                            if nx == no then Impossible
                            else WinX
                        else if oWins then
                            -- If O wins:
                            -- O must have made the last move, so nx must be no.
                            -- If nx > no (i.e., nx == no + 1), it means X played after O won, or O won on X's turn.
                            if nx > no then Impossible
                            else WinO
                        else
                            -- No winner yet:
                            -- If there are no empty cells, it's a draw.
                            if numEmpty == 0 then Draw
                            -- Otherwise, the game is still ongoing.
                            else Ongoing
