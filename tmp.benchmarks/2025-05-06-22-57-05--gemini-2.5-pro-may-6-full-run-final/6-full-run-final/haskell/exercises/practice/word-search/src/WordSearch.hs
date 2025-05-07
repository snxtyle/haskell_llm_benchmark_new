module WordSearch (search, CharPos(..), WordPos(..)) where

data CharPos = CharPos{col::Int, row::Int} deriving (Eq, Show)
data WordPos = WordPos{start::CharPos, end::CharPos} deriving (Eq, Show)

-- Main search function: maps each word to its finding in the grid.
search :: [String] -> [String] -> [(String, Maybe WordPos)]
search grid wordList
  -- Handle empty grid scenarios: if grid is empty or first row is empty, no words can be found.
  | null grid || null (head grid) = map (\word -> (word, Nothing)) wordList
  | otherwise = map (\word -> (word, findFirstOccurrence grid word)) wordList

-- Finds the first occurrence of a single word in the grid.
findFirstOccurrence :: [String] -> String -> Maybe WordPos
findFirstOccurrence grid word
  -- Empty words cannot be meaningfully found.
  | null word = Nothing
  | otherwise =
      let numRows = length grid
          numCols = length (head grid) -- Assumes grid is non-empty and rows are non-empty.

          -- Generate all possible 0-based starting cell coordinates (row, col).
          possibleStartCells = [(r, c) | r <- [0..numRows-1], c <- [0..numCols-1]]
          
          -- Define all 8 directions as (rowChange, colChange) tuples.
          directions = [ (0, 1),  -- Right
                         (0, -1), -- Left
                         (1, 0),  -- Down
                         (-1, 0), -- Up
                         (1, 1),  -- Down-Right (Diagonal)
                         (1, -1), -- Down-Left (Diagonal)
                         (-1, 1), -- Up-Right (Diagonal)
                         (-1, -1) -- Up-Left (Diagonal)
                       ]
          
          -- Use list comprehension (List monad) to search for the word.
          -- It collects all WordPos results from checkDirection.
          foundPositions = do
            (r, c) <- possibleStartCells -- For each potential start cell
            (dr, dc) <- directions       -- For each direction
            -- checkDirection returns [WordPos] (empty or a single element list).
            -- The List monad effectively concatenates these lists.
            checkDirection grid word (r, c) (dr, dc) numRows numCols
            
      -- Return the first found position, if any.
      in case foundPositions of
           (pos:_) -> Just pos -- Found the word, return its position.
           []      -> Nothing  -- Word not found in any cell/direction.

-- Checks if the given 'word' can be formed in the 'grid'
-- starting at (startR, startC) in direction (dr, dc).
-- numRows and numCols are dimensions of the grid.
-- Returns a list: [WordPos] if found (containing one element), or [] if not.
checkDirection :: [String] -> String -> (Int, Int) -> (Int, Int) -> Int -> Int -> [WordPos]
checkDirection grid word (startR, startC) (dr, dc) numRows numCols
  -- This check is defensive; null word should ideally be handled by findFirstOccurrence.
  | null word = [] 
  | otherwise =
      let wordLen = length word
          -- Calculate 0-based end coordinates for the word.
          endR = startR + (wordLen - 1) * dr
          endC = startC + (wordLen - 1) * dc

          -- Helper function to check if a 0-based coordinate (r, c) is within grid boundaries.
          inBounds :: Int -> Int -> Bool
          inBounds r c = r >= 0 && r < numRows && c >= 0 && c < numCols

      -- The starting cell (startR, startC) is known to be in bounds because it's from possibleStartCells.
      -- We only need to check if the calculated end cell (endR, endC) is within bounds.
      -- If the end point is out of bounds, the word cannot exist in this direction.
      in if not (inBounds endR endC)
         then [] -- Word path goes out of grid boundaries.
         else
           -- Extract the characters from the grid along the specified path.
           -- The indices (startR + i * dr, startC + i * dc) are guaranteed to be in bounds
           -- because both start and end points of the path are in bounds.
           let pathChars = [ (grid !! (startR + i * dr)) !! (startC + i * dc) | i <- [0..wordLen-1] ]
               extractedWord = pathChars -- In Haskell, [Char] is an alias for String.
           
           -- If the extracted characters match the word, create and return WordPos.
           in if extractedWord == word
              then
                   -- Convert 0-based (row, col) indices to 1-based CharPos (col, row).
                   let startPos = CharPos (startC + 1) (startR + 1)
                       endPos   = CharPos (endC + 1) (endR + 1)
                   in [WordPos startPos endPos]
              else [] -- Word does not match characters on this path.
