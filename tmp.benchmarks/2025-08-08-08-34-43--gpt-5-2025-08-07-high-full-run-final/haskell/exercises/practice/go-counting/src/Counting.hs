module Counting (
    Color(..),
    territories,
    territoryFor
) where

import Data.Set (Set)
import qualified Data.Set as Set

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int)

-- Public API

territories :: [String] -> [(Set Coord, Maybe Color)]
territories board =
  let
    allCoords :: [Coord]
    allCoords = concat $ zipWith (\y row -> [ (x, y) | x <- [1 .. length row] ])
                                 [1 ..]
                                 board

    step :: (Set Coord, [(Set Coord, Maybe Color)]) -> Coord -> (Set Coord, [(Set Coord, Maybe Color)])
    step (visited, acc) c =
      case cell board c of
        Just ' ' ->
          if c `Set.member` visited
            then (visited, acc)
            else
              let (region, borderColors) = floodFrom board c
                  visited'               = Set.union visited region
                  owner                  = ownerOf borderColors
              in (visited', (region, owner) : acc)
        _ -> (visited, acc)

    (_, resultRev) = foldl step (Set.empty, []) allCoords
  in reverse resultRev

territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board coord =
  case cell board coord of
    Just ' ' ->
      let (region, borderColors) = floodFrom board coord
      in Just (region, ownerOf borderColors)
    _ -> Nothing

-- Internal helpers

ownerOf :: Set Color -> Maybe Color
ownerOf colors
  | Set.size colors == 1 = Just (Set.findMin colors)
  | otherwise            = Nothing

floodFrom :: [String] -> Coord -> (Set Coord, Set Color)
floodFrom board start =
  let
    go :: Set Coord -> Set Coord -> Set Color -> [Coord] -> (Set Coord, Set Color)
    go visited region borders [] = (region, borders)
    go visited region borders (q:qs)
      | q `Set.member` visited = go visited region borders qs
      | otherwise =
          let visited' = Set.insert q visited
              region'  = Set.insert q region
              ns       = neighbors board q
              (qs', borders') =
                foldl
                  (\(que, bs) n ->
                      case cell board n of
                        Just ' ' -> (n : que, bs)
                        Just ch  -> case stoneColor ch of
                                      Just col -> (que, Set.insert col bs)
                                      Nothing  -> (que, bs)
                        Nothing  -> (que, bs)
                  )
                  (qs, borders)
                  ns
          in go visited' region' borders' qs'
  in go Set.empty Set.empty Set.empty [start]

stoneColor :: Char -> Maybe Color
stoneColor 'B' = Just Black
stoneColor 'W' = Just White
stoneColor _   = Nothing

neighbors :: [String] -> Coord -> [Coord]
neighbors board (x, y) =
  filter (inBounds board)
    [ (x - 1, y)
    , (x + 1, y)
    , (x, y - 1)
    , (x, y + 1)
    ]

inBounds :: [String] -> Coord -> Bool
inBounds b (x, y) =
  y >= 1 && y <= length b &&
  let row = b !! (y - 1)
  in x >= 1 && x <= length row

cell :: [String] -> Coord -> Maybe Char
cell b c@(x, y)
  | inBounds b c = Just ((b !! (y - 1)) !! (x - 1))
  | otherwise    = Nothing
