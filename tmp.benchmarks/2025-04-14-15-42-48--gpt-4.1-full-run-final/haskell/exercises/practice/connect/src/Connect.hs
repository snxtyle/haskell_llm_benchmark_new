module Connect (Mark(..), winner) where

import Data.Maybe (mapMaybe)
import Data.List (transpose)
import qualified Data.Set as Set

data Mark = Cross | Nought deriving (Eq, Show)

type Pos = (Int, Int)

-- | Parse the board into a list of rows, each row is a list of Maybe Mark
parseBoard :: [String] -> [[Maybe Mark]]
parseBoard = map parseRow
  where
    parseRow = map charToMark . filter (not . (`elem` " \n\r"))
    charToMark '.' = Nothing
    charToMark 'X' = Just Cross
    charToMark 'O' = Just Nought
    charToMark _   = Nothing

-- | Get the size of the board (rows, cols)
boardSize :: [[a]] -> (Int, Int)
boardSize rows = (length rows, if null rows then 0 else length (head rows))

-- | Get all positions for a given mark
positionsOf :: Mark -> [[Maybe Mark]] -> [Pos]
positionsOf mark board =
  [ (r, c)
  | (r, row) <- zip [0..] board
  , (c, cell) <- zip [0..] row
  , cell == Just mark
  ]

-- | Get neighbors of a position on a hex grid
neighbors :: (Int, Int) -> (Int, Int) -> [Pos]
neighbors (rows, cols) (r, c) =
  filter inBounds
    [ (r-1, c)   -- up
    , (r-1, c+1) -- up-right
    , (r, c-1)   -- left
    , (r, c+1)   -- right
    , (r+1, c-1) -- down-left
    , (r+1, c)   -- down
    ]
  where
    inBounds (r', c') = r' >= 0 && r' < rows && c' >= 0 && c' < cols

-- | Check if a player has connected their two sides
hasWon :: Mark -> [[Maybe Mark]] -> Bool
hasWon mark board =
  let (rows, cols) = boardSize board
      boardMap = \pos -> let (r, c) = pos in board !! r !! c
      -- For Nought (O): top to bottom
      -- For Cross (X): left to right
      (startEdges, goalEdge) = case mark of
        Nought ->
          ( [ (0, c) | c <- [0..cols-1], board !! 0 !! c == Just Nought ]
          , \pos -> fst pos == rows - 1
          )
        Cross ->
          ( [ (r, 0) | r <- [0..rows-1], board !! r !! 0 == Just Cross ]
          , \pos -> snd pos == cols - 1
          )
  in search Set.empty startEdges goalEdge boardMap (rows, cols) mark

-- | BFS/DFS to find a path from any start to the goal edge
search :: Set.Set Pos -> [Pos] -> (Pos -> Bool) -> (Pos -> Maybe Mark) -> (Int, Int) -> Mark -> Bool
search _ [] _ _ _ _ = False
search visited (p:ps) goalEdge boardMap size mark
  | Set.member p visited = search visited ps goalEdge boardMap size mark
  | not (boardMap p == Just mark) = search visited ps goalEdge boardMap size mark
  | goalEdge p = True
  | otherwise =
      let ns = neighbors size p
          new = filter (\n -> not (Set.member n visited) && boardMap n == Just mark) ns
      in search (Set.insert p visited) (new ++ ps) goalEdge boardMap size mark

winner :: [String] -> Maybe Mark
winner boardStrs =
  let board = parseBoard boardStrs
  in case (hasWon Cross board, hasWon Nought board) of
       (True, _)    -> Just Cross
       (_, True)    -> Just Nought
       _            -> Nothing
