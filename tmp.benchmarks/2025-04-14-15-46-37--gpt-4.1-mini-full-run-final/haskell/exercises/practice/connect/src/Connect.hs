module Connect (Mark(..), winner) where

import Data.Maybe (isJust)
import Data.List (transpose)
import qualified Data.Set as Set

data Mark = Cross | Nought deriving (Eq, Show)

-- The board is a list of strings, each string is a row.
-- 'X' means Cross, 'O' means Nought, '.' means empty.
-- Player O tries to connect top to bottom.
-- Player X tries to connect left to right.

type Board = [String]
type Pos = (Int, Int) -- (row, col)

-- Get the mark at a position, if any
markAt :: Board -> Pos -> Maybe Mark
markAt board (r,c)
  | r < 0 || r >= length board = Nothing
  | c < 0 || c >= length (board !! r) = Nothing
  | otherwise = case (board !! r) !! c of
      'X' -> Just Cross
      'O' -> Just Nought
      _   -> Nothing

-- Get neighbors of a hex cell in the board
neighbors :: Pos -> [Pos]
neighbors (r,c) =
  [ (r-1,c), (r-1,c+1)
  , (r,c-1),           (r,c+1)
  , (r+1,c-1), (r+1,c)
  ]

-- Check if a player has connected their sides
-- For O (Nought): connected top (row=0) to bottom (row=height-1)
-- For X (Cross): connected left (col=0) to right (col=width-1)
connected :: Board -> Mark -> Bool
connected board mark =
  let height = length board
      width = if null board then 0 else length (head board)
      -- Starting positions for the player
      starts = case mark of
        Nought -> [(0,c) | c <- [0..width-1], markAt board (0,c) == Just Nought]
        Cross  -> [(r,0) | r <- [0..height-1], markAt board (r,0) == Just Cross]
      -- Target condition for the player
      isTarget (r,c) = case mark of
        Nought -> r == height - 1
        Cross  -> c == width - 1
      -- DFS to find if a path exists from starts to any target
      dfs visited [] = False
      dfs visited (p:ps)
        | isTarget p = True
        | otherwise =
            let nbrs = filter (\pos -> markAt board pos == Just mark && not (Set.member pos visited)) (neighbors p)
                visited' = Set.insert p visited
            in dfs visited' (nbrs ++ ps)
  in dfs Set.empty starts

winner :: [String] -> Maybe Mark
winner board
  | connected board Nought = Just Nought
  | connected board Cross  = Just Cross
  | otherwise              = Nothing
