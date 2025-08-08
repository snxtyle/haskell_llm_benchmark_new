module StateOfTicTacToe (gameState, GameState(..)) where

import Data.List (transpose)

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

gameState :: [String] -> GameState
gameState board
  | not (validShape board) = invalid
  | not (validChars board) = invalid
  | diff < 0 || diff > 1   = invalid
  | xWins && oWins         = invalid
  | xWins && diff /= 1     = invalid
  | oWins && diff /= 0     = invalid
  | xWins                  = WinX
  | oWins                  = WinO
  | empties > 0            = Ongoing
  | otherwise              = Draw
  where
    invalid = Impossible
    b = normalize board
    xs = count 'X' b
    os = count 'O' b
    diff = xs - os
    empties = 9 - xs - os
    xWins = hasWin 'X' b
    oWins = hasWin 'O' b

validShape :: [String] -> Bool
validShape rows = length rows == 3 && all ((== 3) . length) rows

validChars :: [String] -> Bool
validChars rows = all (`elem` allowed) (concat rows)
  where
    allowed = "XO _."

normalize :: [String] -> [[Char]]
normalize = map (map norm)
  where
    norm 'X' = 'X'
    norm 'O' = 'O'
    norm _   = ' '

count :: Char -> [[Char]] -> Int
count p = length . filter (== p) . concat

hasWin :: Char -> [[Char]] -> Bool
hasWin p b = any (all (== p)) (rows3 ++ cols ++ diags)
  where
    rows3 = b
    cols = transpose b
    diags =
      [ [b !! 0 !! 0, b !! 1 !! 1, b !! 2 !! 2]
      , [b !! 0 !! 2, b !! 1 !! 1, b !! 2 !! 0]
      ]
