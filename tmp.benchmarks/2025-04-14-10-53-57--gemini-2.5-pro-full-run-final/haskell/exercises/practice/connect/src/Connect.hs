module Connect (Mark(..), winner) where

import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (nub) -- Using nub to keep queue slightly smaller

data Mark = Cross | Nought deriving (Eq, Show)

type Coord = (Int, Int) -- (row, col)

-- | Represents the player whose turn it is or whose win condition is being checked.
data Player = PlayerX | PlayerO deriving (Eq, Show)

-- | Get the character representation of a player's mark.
playerMarkChar :: Player -> Char
playerMarkChar PlayerX = 'X'
playerMarkChar PlayerO = 'O'

-- | Get the Mark representation of a player.
playerMark :: Player -> Mark
playerMark PlayerX = Cross
playerMark PlayerO = Nought

-- | Determine the winner of the Connect game.
winner :: [String] -> Maybe Mark
winner boardStrings
  -- Handle empty or invalid board (no rows, or first row is empty after filtering)
  | null processedBoard || null (head processedBoard) = Nothing
  | otherwise =
      -- Check if Player X (Cross) has a winning path
      case findPath PlayerX of
        True -> Just (playerMark PlayerX)
        False ->
          -- If X didn't win, check if Player O (Nought) has a winning path
          case findPath PlayerO of
            True -> Just (playerMark PlayerO)
            False -> Nothing -- Neither player has a winning path
  where
    -- Remove spaces from board strings to get the logical grid
    processedBoard :: [[Char]]
    processedBoard = map (filter (/= ' ')) boardStrings

    height :: Int
    height = length processedBoard

    width :: Int
    width = length (head processedBoard)

    -- | Get character at (r, c), returns '.' if out of bounds.
    -- This simplifies neighbor checking by treating invalid coordinates as empty.
    getChar :: Coord -> Char
    getChar (r, c)
      | r >= 0 && r < height && c >= 0 && c < width = (processedBoard !! r) !! c
      | otherwise = '.' -- Treat out of bounds as empty

    -- | Check if a coordinate is on the winning edge for a given player.
    isWinningCoord :: Player -> Coord -> Bool
    isWinningCoord PlayerX (_, c) = c == width - 1  -- X wins if they reach the right edge
    isWinningCoord PlayerO (r, _) = r == height - 1 -- O wins if they reach the bottom edge

    -- | Get the starting coordinates for a player along their starting edge.
    startCoords :: Player -> [Coord]
    startCoords PlayerX = [(r, 0) | r <- [0..height-1], getChar (r, 0) == playerMarkChar PlayerX] -- X starts at left edge (col 0)
    startCoords PlayerO = [(0, c) | c <- [0..width-1], getChar (0, c) == playerMarkChar PlayerO] -- O starts at top edge (row 0)

    -- | Find potential neighbors for a coordinate on the hex grid.
    -- These are the 6 adjacent hex cells.
    potentialNeighbors :: Coord -> [Coord]
    potentialNeighbors (r, c) =
      [ (r, c - 1), (r, c + 1)    -- Left, Right
      , (r - 1, c), (r - 1, c + 1) -- Top-Left, Top-Right
      , (r + 1, c - 1), (r + 1, c) -- Bottom-Left, Bottom-Right
      ]

    -- | Check if a coordinate is within the board boundaries.
    isValidCoord :: Coord -> Bool
    isValidCoord (r, c) = r >= 0 && r < height && c >= 0 && c < width

    -- | Find valid neighbors for a coordinate (filter potential neighbors to be within bounds).
    validNeighbors :: Coord -> [Coord]
    validNeighbors = filter isValidCoord . potentialNeighbors

    -- | Perform a Breadth-First Search (BFS) to find if a player has a winning path.
    -- Returns True if a path is found, False otherwise.
    findPath :: Player -> Bool
    findPath player = bfs Set.empty (startCoords player)
      where
        targetChar = playerMarkChar player -- The character ('X' or 'O') we are looking for

        -- bfs :: visited_cells -> queue_of_cells_to_visit -> found_path?
        bfs :: Set Coord -> [Coord] -> Bool
        bfs _ [] = False -- Queue is empty, no path found from the starting cells
        bfs visited (current:queue)
          -- If the current cell has already been visited, skip it and process the rest of the queue.
          | current `Set.member` visited = bfs visited queue
          -- If the current cell doesn't contain the player's mark (safety check, shouldn't normally happen).
          | getChar current /= targetChar = bfs visited queue
          -- If the current cell is on the player's winning edge, a path is found.
          | isWinningCoord player current = True
          -- Otherwise, explore neighbors.
          | otherwise =
              let -- Add the current cell to the set of visited cells.
                  newVisited = Set.insert current visited
                  -- Find neighbors that are valid, contain the player's mark, and haven't been visited yet.
                  neighborsToExplore = filter (\coord -> getChar coord == targetChar && not (coord `Set.member` newVisited)) (validNeighbors current)
                  -- Add the new neighbors to the end of the queue for BFS.
                  -- Using nub removes duplicates, slightly optimizing queue size.
                  newQueue = queue ++ nub neighborsToExplore
              -- Continue the search with the updated visited set and queue.
              in bfs newVisited newQueue
