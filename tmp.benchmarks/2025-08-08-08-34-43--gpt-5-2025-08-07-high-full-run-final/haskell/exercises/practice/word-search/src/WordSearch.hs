module WordSearch (search, CharPos(..), WordPos(..)) where

import Data.Maybe (listToMaybe)

data CharPos = CharPos { col :: Int, row :: Int } deriving (Eq, Show)
data WordPos = WordPos { start :: CharPos, end :: CharPos } deriving (Eq, Show)

search :: [String] -> [String] -> [(String, Maybe WordPos)]
search grid wordList = map (\w -> (w, findPosition grid w)) wordList

-- Find the first occurrence of a word in the grid (if any), scanning positions
-- in row-major order and testing all 8 directions from each position.
findPosition :: [String] -> String -> Maybe WordPos
findPosition _ [] = Nothing
findPosition grid word =
  listToMaybe
    [ WordPos startPos endPos
    | startPos <- allPositions grid
    , endPos <- maybeToList (matchEnd grid word startPos)
    ]

-- Try matching the word starting at startPos in any of the 8 directions.
-- If a match is found, return the corresponding end position.
matchEnd :: [String] -> String -> CharPos -> Maybe CharPos
matchEnd grid word startPos =
  findFirst
    [ matchInDir grid word startPos dir
    | dir <- directions
    ]

-- Attempt to match the word in a specific direction. If successful, return the end position.
matchInDir :: [String] -> String -> CharPos -> (Int, Int) -> Maybe CharPos
matchInDir grid word (CharPos c r) (dx, dy)
  | null word = Nothing
  | otherwise =
      let coords = [ (c + i * dx, r + i * dy) | i <- [0 .. length word - 1] ]
          fits   = and (zipWith (\(cc, rr) ch -> charAt grid rr cc == Just ch) coords word)
      in if fits
            then let endC = c + (length word - 1) * dx
                     endR = r + (length word - 1) * dy
                 in Just (CharPos endC endR)
            else Nothing

-- Generate all valid starting positions in row-major order (1-based).
allPositions :: [String] -> [CharPos]
allPositions grid =
  concat $
    zipWith
      (\r rowStr -> [ CharPos c r | c <- [1 .. length rowStr] ])
      [1 ..]
      grid

-- All 8 directions: E, SE, S, SW, W, NW, N, NE (dx, dy)
directions :: [(Int, Int)]
directions =
  [ ( 1,  0)  -- East
  , ( 1,  1)  -- SouthEast
  , ( 0,  1)  -- South
  , (-1,  1)  -- SouthWest
  , (-1,  0)  -- West
  , (-1, -1)  -- NorthWest
  , ( 0, -1)  -- North
  , ( 1, -1)  -- NorthEast
  ]

-- Get the character at 1-based (row, col), if within bounds.
charAt :: [String] -> Int -> Int -> Maybe Char
charAt grid r c = do
  rowStr <- safeIndex grid (r - 1)
  safeIndex rowStr (c - 1)

-- Safe indexing into a list.
safeIndex :: [a] -> Int -> Maybe a
safeIndex xs i
  | i < 0 || i >= length xs = Nothing
  | otherwise               = Just (xs !! i)

-- Find the first Just value in a list, if any.
findFirst :: [Maybe a] -> Maybe a
findFirst [] = Nothing
findFirst (x:xs) = case x of
  Just v  -> Just v
  Nothing -> findFirst xs

-- Helper to use in list comprehensions for Maybe values.
maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]
