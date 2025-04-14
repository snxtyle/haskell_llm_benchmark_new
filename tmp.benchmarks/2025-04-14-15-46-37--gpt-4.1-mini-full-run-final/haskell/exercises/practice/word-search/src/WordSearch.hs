module WordSearch (search, CharPos(..), WordPos(..)) where

data CharPos = CharPos{col::Int, row::Int} deriving (Eq, Show)
data WordPos = WordPos{start::CharPos, end::CharPos} deriving (Eq, Show)

-- Directions to search: 8 directions (horizontal, vertical, diagonal)
directions :: [(Int, Int)]
directions = [ (1,0), (-1,0), (0,1), (0,-1)
             , (1,1), (-1,-1), (1,-1), (-1,1)
             ]

-- Check if a position is inside the grid
inBounds :: Int -> Int -> Int -> Int -> Bool
inBounds maxRow maxCol r c = r >= 0 && r < maxRow && c >= 0 && c < maxCol

-- Try to find the word starting at (r,c) in direction (dr,dc)
-- Returns Just (startPos, endPos) if found, Nothing otherwise
findWordFrom :: [String] -> String -> Int -> Int -> (Int, Int) -> Maybe WordPos
findWordFrom grid word r c (dr, dc) =
  let maxRow = length grid
      maxCol = if maxRow > 0 then length (head grid) else 0
      len = length word
      positions = [ (r + i*dr, c + i*dc) | i <- [0..len-1] ]
      -- Check all positions are in bounds
      allInBounds = all (\(rr,cc) -> inBounds maxRow maxCol rr cc) positions
      -- Check letters match
      lettersMatch = all (\(i,(rr,cc)) -> (grid !! rr) !! cc == word !! i) (zip [0..] positions)
  in if allInBounds && lettersMatch
     then Just $ WordPos (CharPos c r) (CharPos (c + (len-1)*dc) (r + (len-1)*dr))
     else Nothing

-- Search for a single word in the grid, return first found position if any
searchWord :: [String] -> String -> Maybe WordPos
searchWord grid word =
  let maxRow = length grid
      maxCol = if maxRow > 0 then length (head grid) else 0
      candidates = [ (r,c) | r <- [0..maxRow-1], c <- [0..maxCol-1], (grid !! r) !! c == head word ]
      found = [ pos | (r,c) <- candidates, dir <- directions, Just pos <- [findWordFrom grid word r c dir] ]
  in case found of
       (pos:_) -> Just pos
       [] -> Nothing

search :: [String] -> [String] -> [(String, Maybe WordPos)]
search grid wordList = [ (w, searchWord grid w) | w <- wordList ]
