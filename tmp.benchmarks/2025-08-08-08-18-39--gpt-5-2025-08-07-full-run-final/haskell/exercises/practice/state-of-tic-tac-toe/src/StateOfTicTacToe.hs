module StateOfTicTacToe (gameState, GameState(..)) where

import Data.List (transpose)

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

data Player = PX | PO deriving (Eq)

gameState :: [String] -> GameState
gameState board =
  case validateAndState board of
    Left _err -> error "Invalid board"
    Right st  -> st

validateAndState :: [String] -> Either String GameState
validateAndState board
  | not (validShape board) = Left "invalid shape"
  | bothWin                = Left "both players cannot win"
  | winX && not (xCount == oCount + 1) = Left "X cannot win with these counts"
  | winO && not (xCount == oCount)     = Left "O cannot win with these counts"
  | not (xCount == oCount || xCount == oCount + 1) = Left "turn order violated"
  | winX                  = Right WinX
  | winO                  = Right WinO
  | xCount + oCount == 9  = Right Draw
  | otherwise             = Right Ongoing
  where
    cells = concat board
    xCount = length (filter (== 'X') cells)
    oCount = length (filter (== 'O') cells)

    b = map (map charToPlayer) board

    winX = hasWin PX b
    winO = hasWin PO b
    bothWin = winX && winO

validShape :: [String] -> Bool
validShape rows = length rows == 3 && all ((== 3) . length) rows

charToPlayer :: Char -> Maybe Player
charToPlayer 'X' = Just PX
charToPlayer 'O' = Just PO
charToPlayer _   = Nothing

hasWin :: Player -> [[Maybe Player]] -> Bool
hasWin p b =
  any (all (== Just p)) (rows ++ cols ++ diags)
  where
    rows = b
    cols = transpose b
    diags =
      [ [b !! 0 !! 0, b !! 1 !! 1, b !! 2 !! 2]
      , [b !! 0 !! 2, b !! 1 !! 1, b !! 2 !! 0]
      ]
