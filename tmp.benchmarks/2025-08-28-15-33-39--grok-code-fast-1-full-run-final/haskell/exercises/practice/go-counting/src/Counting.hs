module Counting (
    Color(..),
    territories,
    territoryFor
) where

import Data.Set (Set)
import qualified Data.Set as Set

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int)

territories :: [String] -> [(Set Coord, Maybe Color)]
territories board = go empties Set.empty []
  where
    h = length board
    w = if null board then 0 else length (head board)
    empties = [(r, c) | r <- [1..h], c <- [1..w], getCell board (r, c) == Just ' ']
    go [] _ acc = acc
    go (coord:coords) visited acc
      | Set.member coord visited = go coords visited acc
      | otherwise = let comp = flood board Set.empty coord
                    in go coords (Set.union visited comp) ((comp, owner board comp) : acc)

territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board coord = case getCell board coord of
  Just ' ' -> let comp = flood board Set.empty coord
              in Just (comp, owner board comp)
  _ -> Nothing

-- Helper functions
getCell :: [String] -> Coord -> Maybe Char
getCell board (r, c) = if r >= 1 && r <= length board && c >= 1 && c <= length (head board)
                       then Just (board !! (r - 1) !! (c - 1))
                       else Nothing

neighbors :: Coord -> [Coord]
neighbors (r, c) = [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]

flood :: [String] -> Set Coord -> Coord -> Set Coord
flood board visited coord
  | Set.member coord visited = visited
  | otherwise = foldl (flood board) (Set.insert coord visited) validNeighbors
  where
    validNeighbors = filter (\p -> getCell board p == Just ' ') (neighbors coord)

owner :: [String] -> Set Coord -> Maybe Color
owner board comp = let adj = getAdjacentStones board comp
                   in if Set.size adj == 1 then Just (Set.findMin adj) else Nothing

getAdjacentStones :: [String] -> Set Coord -> Set Color
getAdjacentStones board comp = Set.fromList
  [ colorFromChar ch
  | coord <- Set.toList comp
  , neigh <- neighbors coord
  , Just ch <- [getCell board neigh]
  , ch /= ' '
  ]

colorFromChar :: Char -> Color
colorFromChar 'B' = Black
colorFromChar 'W' = White
colorFromChar _ = error "Invalid color character"
