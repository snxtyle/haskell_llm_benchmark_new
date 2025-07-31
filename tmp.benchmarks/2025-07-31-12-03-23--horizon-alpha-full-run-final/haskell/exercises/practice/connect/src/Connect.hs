module Connect (Mark(..), winner) where

import Data.Char (isSpace)
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import qualified Data.Sequence as Seq

data Mark = Cross | Nought deriving (Eq, Show)

-- winner determines if Cross (X) has a left-to-right connection
-- or Nought (O) has a top-to-bottom connection.
winner :: [String] -> Maybe Mark
winner rawLines =
  let board = parseBoard rawLines
  in case () of
       _
         | connectsLeftToRight board 'X' -> Just Cross
         | connectsTopToBottom board 'O' -> Just Nought
         | otherwise -> Nothing

-- Parse the textual board into a 2D grid of cells.
-- Each input line may be indented with spaces to form a rhombus when printed.
-- We ignore leading spaces and split on spaces to get cells.
parseBoard :: [String] -> [[Char]]
parseBoard = map parseRow
  where
    parseRow :: String -> [Char]
    parseRow s =
      let trimmed = dropWhile isSpace s
      in mapMaybe tokenToCell (wordsBySpace trimmed)

    wordsBySpace :: String -> [String]
    wordsBySpace = go [] []
      where
        go acc cur [] =
          reverse (if null cur then acc else reverse cur : acc)
        go acc cur (c:cs)
          | isSpace c =
              if null cur
                then go acc [] cs
                else go (reverse cur : acc) [] cs
          | otherwise = go acc (c:cur) cs

    tokenToCell :: String -> Maybe Char
    tokenToCell [c] | c == 'X' || c == 'O' || c == '.' = Just c
    tokenToCell _ = Nothing

-- Board dimensions: rows may vary in length; we only treat in-bounds positions as valid.
type Pos = (Int, Int) -- (row, col)

inBounds :: [[Char]] -> Pos -> Bool
inBounds grid (r, c) =
  r >= 0 && r < length grid && c >= 0 && c < length (grid !! r)

cellAt :: [[Char]] -> Pos -> Char
cellAt grid (r, c) = (grid !! r) !! c

-- Hex neighbors for an "axial" style on a rhombus with rows offset to the right visually.
-- Using standard neighbors for this representation:
-- (r-1,c) (r-1,c+1) (r,c-1) (r,c+1) (r+1,c-1) (r+1,c)
neighbors :: [[Char]] -> Pos -> [Pos]
neighbors grid (r, c) =
  filter (inBounds grid)
    [ (r-1, c)
    , (r-1, c+1)
    , (r,   c-1)
    , (r,   c+1)
    , (r+1, c-1)
    , (r+1, c)
    ]

-- BFS to check connectivity for a given player and start/goal predicates.
connected :: [[Char]] -> Char -> (Pos -> Bool) -> (Pos -> Bool) -> Bool
connected grid piece isStart isGoal =
  let starts = [ p | r <- [0 .. length grid - 1]
                   , c <- [0 .. length (grid !! r) - 1]
                   , let p = (r, c)
                   , isStart p
                   , cellAt grid p == piece
             ]
      bfs visited queue =
        case queue of
          Seq.Empty -> False
          (p Seq.:<| rest) ->
            if isGoal p then True
            else
              let ns = [ q
                       | q <- neighbors(grid) p
                       , S.notMember q visited
                       , inBounds grid q
                       , cellAt grid q == piece
                       ]
                  visited' = foldr S.insert visited ns
              in bfs visited' (rest Seq.>< Seq.fromList ns)
  in case starts of
       [] -> False
       _  -> bfs (S.fromList starts) (Seq.fromList starts)

-- For X: left-to-right. Start cells are those in first column, goal cells are in last column.
connectsLeftToRight :: [[Char]] -> Char -> Bool
connectsLeftToRight grid piece =
  let isStart (r, c) = c == 0
      isGoal  (r, c) = c == (length (grid !! r) - 1)
  in connected grid piece isStart isGoal

-- For O: top-to-bottom. Start cells are those in first row, goal cells are in last row.
connectsTopToBottom :: [[Char]] -> Char -> Bool
connectsTopToBottom grid piece =
  let isStart (r, _) = r == 0
      isGoal  (r, _) = r == (length grid - 1)
  in connected grid piece isStart isGoal
