module WordSearch (search, CharPos(..), WordPos(..)) where

data CharPos = CharPos{col::Int, row::Int} deriving (Eq, Show)
data WordPos = WordPos{start::CharPos, end::CharPos} deriving (Eq, Show)

search :: [String] -> [String] -> [(String, Maybe WordPos)]
search grid wordList = map (\w -> (w, findWord grid w)) wordList

-- Find the first occurrence of a word in the grid, searching in all 8 directions.
findWord :: [String] -> String -> Maybe WordPos
findWord _ [] = Nothing
findWord grid w@(ch:_) =
  case [ WordPos (CharPos (c + 1) (r + 1))
                  (CharPos (c + (lw - 1) * dc + 1) (r + (lw - 1) * dr + 1))
       | r <- [0 .. rows - 1]
       , let rowStr = grid !! r
       , c <- [0 .. length rowStr - 1]
       , rowStr !! c == ch
       , (dr, dc) <- directions
       , matchesFrom grid r c dr dc w
       ] of
    (pos:_) -> Just pos
    []      -> Nothing
  where
    rows = length grid
    lw = length w

-- All 8 possible directions: N, S, W, E, NW, NE, SW, SE
directions :: [(Int, Int)]
directions =
  [ (-1, 0)  -- North
  , ( 1, 0)  -- South
  , ( 0,-1)  -- West
  , ( 0, 1)  -- East
  , (-1,-1)  -- Northwest
  , (-1, 1)  -- Northeast
  , ( 1,-1)  -- Southwest
  , ( 1, 1)  -- Southeast
  ]

-- Check if a word matches starting at (r,c) going in direction (dr,dc).
matchesFrom :: [String] -> Int -> Int -> Int -> Int -> String -> Bool
matchesFrom grid r c dr dc w =
  and [ charAt grid (r + k * dr) (c + k * dc) == Just ch
      | (k, ch) <- zip [0..] w
      ]

-- Safe character access with bounds checking.
charAt :: [String] -> Int -> Int -> Maybe Char
charAt grid r c
  | r < 0 || r >= length grid = Nothing
  | c < 0 || c >= length (grid !! r) = Nothing
  | otherwise = Just ((grid !! r) !! c)
