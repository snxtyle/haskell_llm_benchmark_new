module WordSearch (search, CharPos(..), WordPos(..)) where

import Data.Maybe (catMaybes, listToMaybe)

data CharPos = CharPos{col::Int, row::Int} deriving (Eq, Show)
data WordPos = WordPos{start::CharPos, end::CharPos} deriving (Eq, Show)

-- | Search for a list of words in a grid of letters.
-- Returns a list of tuples, where each tuple contains the word and its position.
-- If a word is not found, its position is Nothing.
search :: [String] -> [String] -> [(String, Maybe WordPos)]
search grid wordList = map (\w -> (w, findWord grid w)) wordList

-- | Find the first occurrence of a single word in the grid.
findWord :: [String] -> String -> Maybe WordPos
findWord grid word
  | null grid || null word = Nothing
  | otherwise = listToMaybe . catMaybes $
      [ checkAt grid word (r, c) (dr, dc)
      | r <- [0..numRows-1]
      , c <- [0..numCols-1]
      , (dr, dc) <- directions
      ]
  where
    numRows = length grid
    numCols = length (head grid)
    directions = [(0,1), (0,-1), (1,0), (-1,0), (1,1), (1,-1), (-1,1), (-1,-1)]

-- | Check if a word exists at a given start position and direction.
-- Returns Just WordPos if found, otherwise Nothing.
checkAt :: [String] -> String -> (Int, Int) -> (Int, Int) -> Maybe WordPos
checkAt grid word (start_r, start_c) (dr, dc) =
  let len = length word
      end_r = start_r + (len - 1) * dr
      end_c = start_c + (len - 1) * dc
      numRows = length grid
      numCols = if null grid then 0 else length (head grid)
      
      isOutOfBounds r' c' = r' < 0 || r' >= numRows || c' < 0 || c' >= numCols
      
      -- Check if the whole word fits and matches
      checkWord i (r, c)
        | i >= len = True -- All characters checked
        | isOutOfBounds r c = False
        | grid !! r !! c /= word !! i = False
        | otherwise = checkWord (i + 1) (r + dr, c + dc)

  in if checkWord 0 (start_r, start_c)
     then Just $ WordPos (CharPos (start_c + 1) (start_r + 1)) (CharPos (end_c + 1) (end_r + 1))
     else Nothing
