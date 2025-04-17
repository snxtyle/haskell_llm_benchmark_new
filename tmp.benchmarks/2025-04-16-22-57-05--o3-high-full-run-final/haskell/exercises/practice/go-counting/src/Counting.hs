module Counting (
    Color(..),
    territories,
    territoryFor
) where

import Data.Set (Set)
import qualified Data.Set as Set

----------------------------------------------------------------
-- Public API
----------------------------------------------------------------

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int)   -- (column, row)   – (1,1) is the top‑left corner

-- | Return all territories on the board together with their owner.
--
--   Each element of the returned list consists of
--     * the set of coordinates belonging to one territory and
--     * the owner of that territory, if it can be determined.
--
--   A territory is owned by a colour iff stones of that colour are the
--   only stones adjacent to the territory.
territories :: [String] -> [(Set Coord, Maybe Color)]
territories board = go allPoints Set.empty []
  where
    height    = length board
    -- enumerate every coordinate that is present on the board
    allPoints = [ (x, y)
                | y <- [1 .. height]
                , let rowLen = length (board !! (y - 1))
                , x <- [1 .. rowLen]
                ]

    go [] _ acc = reverse acc
    go (p:ps) visited acc =
      case charAt board p of
        ' ' | Set.notMember p visited ->
               let (terr, owner) = exploreTerritory board p
                   visited'      = visited `Set.union` terr
               in go ps visited' ((terr, owner) : acc)
        _   -> go ps visited acc

-- | Return the territory (and its owner) that contains the given coordinate.
--   If the coordinate does not refer to an empty intersection, return Nothing.
territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor board coord
  | not (inBounds board coord) = Nothing
  | charAt board coord /= ' '  = Nothing
  | otherwise                  = Just (exploreTerritory board coord)

----------------------------------------------------------------
-- Internal helpers
----------------------------------------------------------------

-- | Flood‑fill starting from the given coordinate, collecting
--   * all connected empty points (the territory) and
--   * the set of colours of stones adjacent to that territory.
--
--   From the second piece of information we can infer the owner.
exploreTerritory :: [String] -> Coord -> (Set Coord, Maybe Color)
exploreTerritory board start = bfs Set.empty Set.empty [start]
  where
    bfs territory adjColors [] =
      (territory, owner adjColors)

    bfs territory adjColors (p:queue)
      | p `Set.member` territory = bfs territory adjColors queue
      | otherwise =
          case charAt board p of
            ' ' ->
              let territory' = Set.insert p territory
                  next       = filter (inBounds board) (neighbors p)
              in bfs territory' adjColors (next ++ queue)
            'B' -> bfs territory (Set.insert Black adjColors) queue
            'W' -> bfs territory (Set.insert White adjColors) queue
            _   -> bfs territory adjColors queue

    owner cs =
      case Set.toList cs of
        [Black] -> Just Black
        [White] -> Just White
        _       -> Nothing

-- | Orthogonal neighbours of a coordinate.
neighbors :: Coord -> [Coord]
neighbors (x, y) =
  [ (x - 1, y)   -- left
  , (x + 1, y)   -- right
  , (x, y - 1)   -- up
  , (x, y + 1)   -- down
  ]

-- | Test whether a coordinate lies within the bounds of the board.
inBounds :: [String] -> Coord -> Bool
inBounds board (x, y)
  | y < 1 || y > length board = False
  | x < 1 || x > rowLen       = False
  | otherwise                 = True
  where
    rowLen = length (board !! (y - 1))

-- | Look up the character at a valid coordinate.
charAt :: [String] -> Coord -> Char
charAt board (x, y) = (board !! (y - 1)) !! (x - 1)
