module Connect (Mark(..), winner) where

import qualified Data.Set as Set

data Mark = Cross | Nought deriving (Eq, Show)

type Pos = (Int, Int)

winner :: [String] -> Maybe Mark
winner board =
  let grid = map (filter (/= ' ')) board
  in case () of
       _ | wins 'X' grid -> Just Cross
         | wins 'O' grid -> Just Nought
         | otherwise     -> Nothing

-- Determine if the given player character ('X' or 'O') has a winning connection
wins :: Char -> [String] -> Bool
wins ch grid
  | null grid = False
  | otherwise =
      let h = length grid
          rowLen r = length (grid !! r)
          cell r c = (grid !! r) !! c

          starts = case ch of
            'X' -> [(r, 0) | r <- [0 .. h - 1], rowLen r > 0, cell r 0 == ch]
            'O' ->
              let l0 = if h > 0 then rowLen 0 else 0
              in [(0, c) | c <- [0 .. l0 - 1], l0 > 0, cell 0 c == ch]
            _ -> []

          isGoal = case ch of
            'X' -> \(r, c) -> let lr = rowLen r in lr > 0 && c == lr - 1
            'O' -> \(r, _) -> r == h - 1
            _   -> const False
      in search grid ch starts isGoal

-- Depth-first search over hex neighbors
search :: [String] -> Char -> [Pos] -> (Pos -> Bool) -> Bool
search grid ch starts isGoal = go Set.empty starts
  where
    h = length grid
    rowLen r = length (grid !! r)
    inBounds (r, c) = r >= 0 && r < h && c >= 0 && c < rowLen r
    matches (r, c) = (grid !! r) !! c == ch

    neighbors (r, c) =
      filter inBounds
        [ (r - 1, c)
        , (r - 1, c + 1)
        , (r,     c - 1)
        , (r,     c + 1)
        , (r + 1, c - 1)
        , (r + 1, c)
        ]

    go _ [] = False
    go visited (p:stack)
      | Set.member p visited = go visited stack
      | not (inBounds p)     = go visited stack
      | not (matches p)      = go visited stack
      | isGoal p             = True
      | otherwise =
          let visited' = Set.insert p visited
              next = filter (\q -> not (Set.member q visited') && inBounds q && matches q) (neighbors p)
          in go visited' (next ++ stack)
