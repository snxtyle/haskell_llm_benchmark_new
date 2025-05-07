module WordSearch (search, CharPos(..), WordPos(..)) where

import Data.List (find)
import Data.Maybe (listToMaybe)

data CharPos = CharPos{col::Int, row::Int} deriving (Eq, Show)
data WordPos = WordPos{start::CharPos, end::CharPos} deriving (Eq, Show)

-- | Main search function. Maps each word in the word list to its found position (or Nothing).
search :: [String] -> [String] -> [(String, Maybe WordPos)]
search grid wordList = map (\word -> (word, findWord grid word)) wordList

-- | Finds the first occurrence of a word in the grid.
findWord :: [String] -> String -> Maybe WordPos
findWord grid word
  | null word || null grid || null (head grid) = Nothing -- Handle empty word or grid
  | otherwise = listToMaybe foundPositions -- Return the first found position, if any
  where
    height = length grid
    width = length (head grid)
    -- Generate all possible starting coordinates (0-based row, col)
    coords = [(r, c) | r <- [0..height-1], c <- [0..width-1]]
    -- Define the 8 direction vectors (dr, dc) for row and column changes
    deltas = [(dr, dc) | dr <- [-1..1], dc <- [-1..1], (dr, dc) /= (0, 0)]

    -- List comprehension to find all valid positions for the word
    foundPositions = [ pos | (r, c) <- coords, -- For each starting cell
                             (dr, dc) <- deltas, -- For each direction
                             Just pos <- [checkDirection grid word (r, c) (dr, dc)] ] -- Check if word exists

-- | Checks if a word exists starting at (startRow, startCol) in direction (dr, dc).
checkDirection :: [String] -> String -> (Int, Int) -> (Int, Int) -> Maybe WordPos
checkDirection grid word (startRow, startCol) (dr, dc) =
  let
    height = length grid
    width = length (head grid)
    len = length word

    -- Function to safely get a character at 0-based (r, c) coordinates
    getCharAt :: Int -> Int -> Maybe Char
    getCharAt r c
      | r >= 0 && r < height && c >= 0 && c < width = Just $ (grid !! r) !! c
      | otherwise = Nothing

    -- Check if the character at index 'i' of the word matches the grid character
    -- at the corresponding position along the direction.
    matchChar :: Int -> Bool
    matchChar i =
      let r = startRow + i * dr
          c = startCol + i * dc
      in case getCharAt r c of
           Just gridChar -> gridChar == (word !! i) -- Compare grid char with word char
           Nothing       -> False -- Position is out of bounds

    -- Calculate the 0-based end coordinates
    endRow = startRow + (len - 1) * dr
    endCol = startCol + (len - 1) * dc

    -- Check if the end position is within bounds (basic check)
    isEndInBounds = endRow >= 0 && endRow < height && endCol >= 0 && endCol < width

  -- Check if the end position is valid and all characters match
  in if isEndInBounds && all matchChar [0..len-1]
     then Just WordPos { -- Convert 0-based coords to 1-based for CharPos
                         start = CharPos { col = startCol + 1, row = startRow + 1 },
                         end   = CharPos { col = endCol + 1,   row = endRow + 1 }
                       }
     else Nothing
