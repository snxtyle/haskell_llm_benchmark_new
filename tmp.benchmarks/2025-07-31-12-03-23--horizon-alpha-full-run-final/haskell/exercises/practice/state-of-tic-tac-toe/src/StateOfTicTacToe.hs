module StateOfTicTacToe (gameState, GameState(..)) where

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

-- | Determine the game state of a tic-tac-toe board.
-- The board is given as a list of 3 strings, each 3 characters long.
-- Valid characters are 'X', 'O', and ' ' (space) for empty.
-- X starts. If invalid, throws an error with a message indicating why.
gameState :: [String] -> GameState
gameState board =
  let b = normalize board
      xs = count 'X' b
      os = count 'O' b
      winX = hasWin 'X' b
      winO = hasWin 'O' b
  in
    -- Validate counts (turn order)
    case () of
      _ | not (validShape board) ->
            error "Invalid board: must be 3 rows of length 3."
        | not (validChars board) ->
            error "Invalid board: only 'X', 'O', and ' ' are allowed."
        | xs < os ->
            error "Invalid board: wrong turn order."
        | xs - os > 1 ->
            error "Invalid board: X went twice"
        | winX && winO ->
            error "Invalid board: both players cannot win."
        | winX && xs == os ->
            -- If X wins, X must have made the last move so xs == os + 1
            error "Invalid board: game continued or wrong turn order after X already won."
        | winO && xs /= os ->
            -- If O wins, counts must be equal
            error "Invalid board: wrong turn order for O win."
        | winX -> WinX
        | winO -> WinO
        | xs + os == 9 -> Draw
        | otherwise -> Ongoing

-- Helpers

validShape :: [String] -> Bool
validShape rs = length rs == 3 && all ((==3) . length) rs

validChars :: [String] -> Bool
validChars = all (all (`elem` "XO "))

normalize :: [String] -> [String]
normalize rs
  | validShape rs = rs
  | otherwise = rs -- we'll catch invalid shape earlier; keep as-is

count :: Char -> [String] -> Int
count c = sum . map (length . filter (== c))

-- Determine if the given player has a 3-in-a-row
hasWin :: Char -> [String] -> Bool
hasWin p b =
  any (all (== p)) (rows b)
    || any (all (== p)) (cols b)
    || any (all (== p)) (diags b)

rows :: [String] -> [[Char]]
rows = id

cols :: [String] -> [[Char]]
cols [a,b,c] = [[a!!0,b!!0,c!!0],[a!!1,b!!1,c!!1],[a!!2,b!!2,c!!2]]
cols _ = []

diags :: [String] -> [[Char]]
diags [a,b,c] = [[a!!0,b!!1,c!!2],[a!!2,b!!1,c!!0]]
diags _ = []
