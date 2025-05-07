module WordSearch (search, CharPos(..), WordPos(..)) where

import Data.Maybe (listToMaybe)

data CharPos = CharPos { col :: Int, row :: Int } deriving (Eq, Show)
data WordPos = WordPos { start :: CharPos, end :: CharPos } deriving (Eq, Show)

-- | For each word, attempt to find it in any of the 8 directions.
--   Returns the first match (with 1-based coordinates) or Nothing.
search :: [String] -> [String] -> [(String, Maybe WordPos)]
search grid wordList = map (\w -> (w, findWord grid w)) wordList

findWord :: [String] -> String -> Maybe WordPos
findWord grid word = listToMaybe
  [ WordPos (CharPos (c0 + 1) (r0 + 1)) (CharPos (c1 + 1) (r1 + 1))
  | r0 <- [0 .. numRows - 1]
  , c0 <- [0 .. numCols - 1]
  , (dx, dy) <- directions
  , let len = length word
        r1  = r0 + dy * (len - 1)
        c1  = c0 + dx * (len - 1)
  , r1 >= 0, r1 < numRows
  , c1 >= 0, c1 < numCols
  , and [ grid !! (r0 + dy * i) !! (c0 + dx * i) == word !! i
        | i <- [0 .. len - 1]
        ]
  ]
  where
    numRows   = length grid
    numCols   = if null grid then 0 else length (head grid)
    -- (dx, dy): dx = Δcol, dy = Δrow
    directions =
      [ (dx, dy)
      | dx <- [-1, 0, 1]
      , dy <- [-1, 0, 1]
      , (dx, dy) /= (0, 0)
      ]
