module StateOfTicTacToe (gameState, GameState(..)) where

import Data.List (transpose)

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

-- Check if a player has won
hasWon :: Char -> [String] -> Bool
hasWon c board =
  let rows = board
      cols = transpose board
      diag1 = [board !! i !! i | i <- [0..2]]
      diag2 = [board !! i !! (2 - i) | i <- [0..2]]
      lineWin line = all (== c) line
  in any lineWin rows || any lineWin cols || lineWin diag1 || lineWin diag2

-- Count occurrences of a character on the board
countChar :: Char -> [String] -> Int
countChar ch = sum . map (length . filter (== ch))

-- Check if the board is full (no empty spaces)
isFull :: [String] -> Bool
isFull = all (all (/= ' '))

-- Validate the board for turn order and game state consistency
validateBoard :: [String] -> Bool
validateBoard board =
  let xCount = countChar 'X' board
      oCount = countChar 'O' board
      xWin = hasWon 'X' board
      oWin = hasWon 'O' board
      diff = xCount - oCount
      -- X always starts, so xCount == oCount or xCount == oCount + 1
      validTurnOrder = diff == 0 || diff == 1
      -- Both players cannot win simultaneously
      noDoubleWin = not (xWin && oWin)
      -- If X won, xCount must be exactly one more than oCount
      xWinValid = not xWin || diff == 1
      -- If O won, xCount must be equal to oCount
      oWinValid = not oWin || diff == 0
      -- If game ended (win), no moves after that
      -- So if win detected, the board must not have extra moves after that
      -- This is already checked by xWinValid and oWinValid with turn counts
  in validTurnOrder && noDoubleWin && xWinValid && oWinValid

gameState :: [String] -> GameState
gameState board
  | length board /= 3 || any ((/= 3) . length) board = error "Invalid board size"
  | not (all (all (`elem` "XO ")) board) = error "Invalid characters on board"
  | not (validateBoard board) = error "Impossible"
  | hasWon 'X' board = WinX
  | hasWon 'O' board = WinO
  | isFull board = Draw
  | otherwise = Ongoing
