module StateOfTicTacToe (gameState, GameState(..)) where

import Data.List (transpose)

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

type Cell = Char
type Board = [[Cell]]

-- | Convert a flat list of 9 cells into a 3x3 board
toBoard :: [String] -> Board
toBoard xs
  | length xs /= 3 = error "Board must have 3 rows"
  | any (/=3) (map length xs) = error "Each row must have 3 columns"
  | otherwise = xs

-- | All possible lines (rows, columns, diagonals) on the board
linesOfBoard :: Board -> [[Cell]]
linesOfBoard b = rows ++ cols ++ diags
  where
    rows = b
    cols = transpose b
    diags = [[b !! i !! i | i <- [0..2]],
             [b !! i !! (2-i) | i <- [0..2]]]

-- | Count Xs and Os
countXO :: Board -> (Int, Int)
countXO b = (count 'X', count 'O')
  where
    count c = length [cell | row <- b, cell <- row, cell == c]

-- | Find winner, if any
winner :: Board -> Maybe GameState
winner b
  | any (all (=='X')) ls = Just WinX
  | any (all (=='O')) ls = Just WinO
  | otherwise = Nothing
  where
    ls = linesOfBoard b

-- | Check if the board is full
isFull :: Board -> Bool
isFull b = all (\cell -> cell == 'X' || cell == 'O') [cell | row <- b, cell <- row]

-- | Main function
gameState :: [String] -> GameState
gameState xs = 
  let board = toBoard xs
      (xCount, oCount) = countXO board
      win = winner board
      xWins = win == Just WinX
      oWins = win == Just WinO
      full = isFull board
  in
    -- Invalid: wrong number of Xs and Os
    if oCount > xCount || xCount - oCount > 1 then
      Impossible
    -- Invalid: both players win
    else if xWins && oWins then
      Impossible
    -- Invalid: O wins but X has more moves
    else if oWins && xCount /= oCount then
      Impossible
    -- Invalid: X wins but X and O have same number of moves
    else if xWins && xCount /= oCount + 1 then
      Impossible
    -- X wins
    else if xWins then WinX
    -- O wins
    else if oWins then WinO
    -- Draw
    else if full then Draw
    -- Ongoing
    else Ongoing
