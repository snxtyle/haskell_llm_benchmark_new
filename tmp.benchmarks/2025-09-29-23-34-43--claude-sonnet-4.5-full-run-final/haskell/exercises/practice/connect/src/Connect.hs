module Connect (Mark(..), winner) where

import Data.Set (Set)
import qualified Data.Set as Set

data Mark = Cross | Nought deriving (Eq, Show)

winner :: [String] -> Maybe Mark
winner board
  | null board = Nothing
  | hasWon Nought board = Just Nought
  | hasWon Cross board = Just Cross
  | otherwise = Nothing

hasWon :: Mark -> [String] -> Bool
hasWon mark board = case mark of
  Nought -> connectsTopBottom board
  Cross -> connectsLeftRight board

-- Player O (Nought) wins by connecting top to bottom
connectsTopBottom :: [String] -> Bool
connectsTopBottom board =
  let positions = parseBoard board 'O'
      topRow = filter (\(r, _) -> r == 0) positions
      height = length board
  in any (\start -> canReach positions start height isBottom) topRow
  where
    isBottom (r, _) = r == length board - 1

-- Player X (Cross) wins by connecting left to right
connectsLeftRight :: [String] -> Bool
connectsLeftRight board =
  let positions = parseBoard board 'X'
      leftCol = filter (\(_, c) -> c == 0) positions
      width = if null board then 0 else maximum [length row | row <- board]
  in any (\start -> canReach positions start width isRight) leftCol
  where
    isRight (_, c) = c == maximum [length row - 1 | row <- board, not (null row)]

-- Parse board and return positions of the given character
parseBoard :: [String] -> Char -> [(Int, Int)]
parseBoard board char =
  [ (row, col)
  | (row, line) <- zip [0..] board
  , (col, c) <- zip [0..] (filter (/= ' ') line)
  , c == char
  ]

-- Check if we can reach a target position from start using BFS
canReach :: [(Int, Int)] -> (Int, Int) -> Int -> ((Int, Int) -> Bool) -> Bool
canReach positions start _ isTarget =
  let posSet = Set.fromList positions
  in bfs posSet (Set.singleton start) Set.empty isTarget

bfs :: Set (Int, Int) -> Set (Int, Int) -> Set (Int, Int) -> ((Int, Int) -> Bool) -> Bool
bfs posSet frontier visited isTarget
  | Set.null frontier = False
  | any isTarget frontier = True
  | otherwise =
      let newVisited = Set.union visited frontier
          neighbors = Set.unions [getNeighbors posSet pos | pos <- Set.toList frontier]
          newFrontier = Set.difference neighbors newVisited
      in bfs posSet newFrontier newVisited isTarget

-- Get valid neighbors of a position (hexagonal grid)
getNeighbors :: Set (Int, Int) -> (Int, Int) -> Set (Int, Int)
getNeighbors posSet (r, c) =
  let candidates = [ (r - 1, c), (r - 1, c + 1)  -- top-left, top-right
                   , (r, c - 1), (r, c + 1)      -- left, right
                   , (r + 1, c - 1), (r + 1, c)  -- bottom-left, bottom-right
                   ]
  in Set.fromList [pos | pos <- candidates, Set.member pos posSet]
