module Counting (
    Color(..),
    territories,
    territoryFor
) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (mapMaybe)

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int)

-- Public API

territories :: [String] -> [(Set Coord, Maybe Color)]
territories board = go Set.empty [] (allCoords board)
  where
    go _ acc [] = reverse acc
    go visited acc (c:cs)
      | emptyAt board c && not (Set.member c visited) =
          let (region, adjColors) = exploreEmpty board c
              owner = ownerFrom adjColors
              visited' = visited `Set.union` region
          in go visited' ((region, owner) : acc) cs
      | otherwise = go visited acc cs

territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board coord =
  case charAt board coord of
    Just ' ' ->
      let (region, adjColors) = exploreEmpty board coord
      in Just (region, ownerFrom adjColors)
    _ -> Nothing

-- Helpers

ownerFrom :: Set Color -> Maybe Color
ownerFrom s =
  case Set.toList s of
    [c] -> Just c
    _   -> Nothing

exploreEmpty :: [String] -> Coord -> (Set Coord, Set Color)
exploreEmpty board start = go Set.empty Set.empty [start]
  where
    go seen adj [] = (seen, adj)
    go seen adj (p:ps)
      | Set.member p seen = go seen adj ps
      | not (emptyAt board p) = go seen adj ps
      | otherwise =
          let ns       = neighbors p
              emptyNs  = [n | n <- ns, emptyAt board n]
              adjColors = Set.fromList (mapMaybe (colorAt board) ns)
              seen'    = Set.insert p seen
              adj'     = adj `Set.union` adjColors
          in go seen' adj' (emptyNs ++ ps)

neighbors :: Coord -> [Coord]
neighbors (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

emptyAt :: [String] -> Coord -> Bool
emptyAt board c = charAt board c == Just ' '

colorAt :: [String] -> Coord -> Maybe Color
colorAt board c =
  case charAt board c of
    Just 'B' -> Just Black
    Just 'X' -> Just Black -- be permissive if tests use X/O
    Just 'W' -> Just White
    Just 'O' -> Just White
    _        -> Nothing

charAt :: [String] -> Coord -> Maybe Char
charAt rows (x,y) = do
  row <- index1 rows y
  index1 row x

index1 :: [a] -> Int -> Maybe a
index1 xs i
  | i < 1          = Nothing
  | i > length xs  = Nothing
  | otherwise      = Just (xs !! (i - 1))

allCoords :: [String] -> [Coord]
allCoords rows = concat
  [ [(x,y) | x <- [1 .. length row]]
  | (y, row) <- zip [1..] rows
  ]
