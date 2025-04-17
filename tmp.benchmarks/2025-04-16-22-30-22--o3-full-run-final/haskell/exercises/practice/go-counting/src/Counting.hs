module Counting (
    Color(..),
    territories,
    territoryFor
) where

import           Data.Set (Set)
import qualified Data.Set as Set

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int)          -- (row, column) – both 1‑based.
type Board = [String]

-- | Return EVERY territory on the board together with its owner.
--
--   A territory is a (maximally) connected group of empty intersections.
--   Connection is 4‑directional (up, down, left, right).
--
--   The owner is:
--     • Just Black  – if ONLY black stones touch the territory
--     • Just White  – if ONLY white stones touch the territory
--     • Nothing     – if both colours (or none) touch the territory
--
--   The returned list contains one element for every territory
--   (order is irrelevant and left unspecified by the specification).
territories :: Board -> [(Set Coord, Maybe Color)]
territories board =
    go allCoords Set.empty []
  where
    h         = length board
    w         = if null board then 0 else length (head board)
    allCoords = [(r, c) | r <- [1 .. h], c <- [1 .. w]]

    go [] _ acc = acc
    go (p:ps) seen acc
        | p `Set.member` seen           = go ps seen acc
        | charAt board p /= ' '         = go ps seen acc   -- not empty → not a territory start
        | otherwise                     =
            let (terr, owner) = flood board p
                seen'          = Set.union seen terr
            in go ps seen' ((terr, owner) : acc)

-- | Return the territory containing the given coordinate.
--   If the coordinate is outside the board OR not an empty intersection,
--   return Nothing.
territoryFor :: Board -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board p
    | not (inside board p)   = Nothing
    | charAt board p /= ' '  = Nothing
    | otherwise              = Just (flood board p)

----------------------------------------------------------------------
-- Internal helpers
----------------------------------------------------------------------

-- Perform a flood‑fill starting from an empty intersection.
-- Result:
--   * Set Coord         – every point belonging to the territory
--   * Maybe Color       – the owner (per the rules above)
flood :: Board -> Coord -> (Set Coord, Maybe Color)
flood board start = go (Set.singleton start) [start] Set.empty
  where
    go terr [] touchingColours =
        (terr, determineOwner touchingColours)
    go terr (p:stack) touchingColours =
        let (empties, borderingStones) = partitionNeighbors board p
            newTerr  = Set.union terr empties
            newStack = [q | q <- Set.toList empties, q `Set.notMember` terr] ++ stack
            newTouch = Set.union touchingColours borderingStones
        in go newTerr newStack newTouch

-- Given a coordinate known to be empty, look at its neighbours.
-- Return:
--   * Set of neighbouring empty coords (for flood‑fill continuation)
--   * Set of colours of stones that touch the coordinate
partitionNeighbors :: Board -> Coord -> (Set Coord, Set Color)
partitionNeighbors board p =
    foldr classify (Set.empty, Set.empty) (neighbors board p)
  where
    classify q (empties, colours) =
        case charAt board q of
            ' ' -> (Set.insert q empties, colours)
            'B' -> (empties, Set.insert Black colours)
            'W' -> (empties, Set.insert White colours)
            _   -> (empties, colours)           -- unknown char (shouldn't happen)

-- Decide owner by the set of touching colours.
determineOwner :: Set Color -> Maybe Color
determineOwner s
    | Set.size s == 1 = Just (Set.findMin s)
    | otherwise       = Nothing

----------------------------------------------------------------------
-- Coordinate / board helpers
----------------------------------------------------------------------

-- All four orthogonal neighbours that are inside the board.
neighbors :: Board -> Coord -> [Coord]
neighbors board (r, c) =
    filter (inside board)
        [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]

-- Check whether a coordinate lies within the board boundaries.
inside :: Board -> Coord -> Bool
inside board (r, c) =
    r >= 1 && r <= h && c >= 1 && c <= w
  where
    h = length board
    w = if null board then 0 else length (head board)

-- Character at a coordinate (assumes coord is inside and indices are 1‑based).
charAt :: Board -> Coord -> Char
charAt b (r, c) = (b !! (r - 1)) !! (c - 1)
