module StateOfTicTacToe (gameState, GameState(..)) where

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

gameState :: [String] -> GameState
gameState board =
  let
    -- Count occurrences of a given mark
    count c = sum [ length (filter (== c) row) | row <- board ]

    xs = count 'X'
    os = count 'O'

    -- All possible winning lines: rows, columns, and diagonals
    rows = board
    cols = [ [ row !! i | row <- board ] | i <- [0..2] ]
    diags =
      [ [ board !! i !! i         | i <- [0..2] ]
      , [ board !! i !! (2 - i)   | i <- [0..2] ]
      ]
    winLines = rows ++ cols ++ diags

    xWins = any (all (== 'X')) winLines
    oWins = any (all (== 'O')) winLines
  in
    -- Invalid count of turns
    if os > xs || xs > os + 1
    then Impossible

    -- Both players winning is impossible
    else if xWins && oWins
    then Impossible

    -- X wins must have one extra X
    else if xWins
    then if xs == os + 1 then WinX else Impossible

    -- O wins must have equal counts
    else if oWins
    then if xs == os then WinO else Impossible

    -- If no winner but full board -> Draw
    else if xs + os == 9
    then Draw

    -- Otherwise still in progress
    else Ongoing
