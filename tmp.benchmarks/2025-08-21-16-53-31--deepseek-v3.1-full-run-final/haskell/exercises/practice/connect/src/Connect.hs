module Connect (Mark(..), winner) where

import Data.Maybe (isJust, fromMaybe)
import Data.List (find)

data Mark = Cross | Nought deriving (Eq, Show)

winner :: [String] -> Maybe Mark
winner board
  | hasWon 'O' board = Just Nought
  | hasWon 'X' board = Just Cross
  | otherwise = Nothing

hasWon :: Char -> [String] -> Bool
hasWon player board = isJust $ find (connects player board) startingPoints
  where
    startingPoints = case player of
      'O' -> [(0, col) | col <- [0..maxCol board 0], getCell board 0 col == Just 'O']
      'X' -> [(row, 0) | row <- [0..length board - 1], getCell board row 0 == Just 'X']
      _   -> []

connects :: Char -> [String] -> (Int, Int) -> Bool
connects player board start = dfs start []
  where
    height = length board
    
    dfs (r, c) visited
      | reachedGoal (r, c) = True
      | (r, c) `elem` visited = False
      | otherwise = case getCell board r c of
          Just cell -> if cell == player
            then any (dfs' (r, c)) (neighbors r c)
            else False
          Nothing -> False
      where
        dfs' pos (nr, nc) = dfs (nr, nc) (pos:visited)
    
    reachedGoal (r, c) = case player of
      'O' -> r == height - 1
      'X' -> let maxColInRow = maxCol board r in c == maxColInRow
      _   -> False

-- Get the maximum column index for a given row (accounting for spaces)
maxCol :: [String] -> Int -> Int
maxCol board row
  | row < 0 || row >= length board = -1
  | otherwise = length (board !! row) - 1

-- Get cell value at position, handling the board structure
getCell :: [String] -> Int -> Int -> Maybe Char
getCell board row col
  | row < 0 || row >= length board = Nothing
  | col < 0 || col >= length (board !! row) = Nothing
  | otherwise = 
      let cell = board !! row !! col
      in if cell == ' ' then Nothing else Just cell

-- Get neighbors for a hexagonal grid
neighbors :: Int -> Int -> [(Int, Int)]
neighbors row col =
  let evenRow = even row
      -- For even rows, neighbors are at different offsets than odd rows
      hexNeighbors = if evenRow
        then [ (row-1, col), (row-1, col+1)   -- top-left, top-right
             , (row, col-1), (row, col+1)     -- left, right
             , (row+1, col), (row+1, col+1) ] -- bottom-left, bottom-right
        else [ (row-1, col-1), (row-1, col)   -- top-left, top-right
             , (row, col-1), (row, col+1)     -- left, right
             , (row+1, col-1), (row+1, col) ] -- bottom-left, bottom-right
  in hexNeighbors
