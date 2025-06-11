module Counting (
    Color(..),
    territories,
    territoryFor
) where

import           Data.Set (Set)
import qualified Data.Set as S

-- | Colour of a stone.
data Color = Black | White deriving (Eq, Ord, Show)

-- NOTE: The coordinate convention used by the Exercism tests is
-- (column, row) where the origin (1,1) is the top-left corner and
--   • the first component grows *horizontally*  (to the right)
--   • the second component grows *vertically*   (downwards)
--
-- This is the opposite of the conventional (row,column) order that the
-- previous implementation assumed.  All functions have been updated to
-- follow the required (x,y) ordering.
type Coord = (Int, Int)         -- ^ (x , y) = (column , row) – both 1-based.

-- | Public API ---------------------------------------------------------------

-- | Return every empty territory on the board together with its owner.
--
--   If a territory is not owned by exactly one colour, the owner is Nothing.
territories :: [String] -> [(Set Coord, Maybe Color)]
territories board =
    go allCoords S.empty []
  where
    rows      = length board
    cols      = case board of
                  []    -> 0
                  (r:_) -> length r
    -- Enumerate all coordinates (x,y) row-major.
    allCoords = [(x,y) | y <- [1..rows], x <- [1..cols]]

    go [] _ acc = reverse acc
    go (p:ps) visited acc
        | p `S.member` visited           = go ps visited acc
        | Just ' ' <- charAt board p     =
              let (terr, owners) = exploreTerritory board p
                  visited'       = visited `S.union` terr
              in  go ps visited' ((terr, ownerFrom owners) : acc)
        | otherwise                      = go ps visited acc

-- | Return the territory (and owner) that contains the given coordinate.
--   Returns Nothing when the coordinate is not an empty point.
territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board coord
    | Just ' ' <- charAt board coord =
        let (terr, owners) = exploreTerritory board coord
        in  Just (terr, ownerFrom owners)
    | otherwise                      = Nothing

-- | Internal helpers ---------------------------------------------------------

-- Perform a flood-fill from a given empty coordinate, returning:
--   * The set of all connected empty coordinates.
--   * The set of colours of stones adjacent to that empty region.
exploreTerritory :: [String] -> Coord -> (Set Coord, Set Color)
exploreTerritory board start = bfs (S.singleton start) [start] S.empty
  where
    bfs terr [] owns = (terr, owns)
    bfs terr (p:queue) owns =
        let (terr', queue', owns') = foldr step (terr, queue, owns) (neighbors p)
        in  bfs terr' queue' owns'

    step n (terrAcc, queueAcc, ownAcc)
        | Just ' '  <- charAt board n
        , n `S.notMember` terrAcc      = ( S.insert n terrAcc
                                         , n : queueAcc
                                         , ownAcc)
        | Just 'B' <- charAt board n  = (terrAcc, queueAcc, S.insert Black ownAcc)
        | Just 'W' <- charAt board n  = (terrAcc, queueAcc, S.insert White ownAcc)
        | otherwise                   = (terrAcc, queueAcc, ownAcc)

-- | Determine the unique owner (if any) from a set of adjacent colours.
ownerFrom :: Set Color -> Maybe Color
ownerFrom owners
    | S.size owners == 1 = Just (S.findMin owners)
    | otherwise          = Nothing

-- | Board access -------------------------------------------------------------

-- | Character at a coord or Nothing when outside the board.
--   The coordinate convention is (x,y) = (column,row).
charAt :: [String] -> Coord -> Maybe Char
charAt board (x,y)
    | y >= 1 && y <= length board
    , let row = board !! (y - 1)
    , x >= 1 && x <= length row        = Just (row !! (x - 1))
    | otherwise                        = Nothing

-- | Orthogonal neighbours (left, right, up, down) for (x,y).
neighbors :: Coord -> [Coord]
neighbors (x,y) = [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]
