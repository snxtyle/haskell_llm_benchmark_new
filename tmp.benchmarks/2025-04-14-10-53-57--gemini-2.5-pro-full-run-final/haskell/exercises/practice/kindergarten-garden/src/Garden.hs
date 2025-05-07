module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import Data.List (sort)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

-- Use a Map to store the plants for each student.
-- Using newtype for efficiency as there's only one constructor with one field.
newtype Garden = Garden (Map.Map String [Plant]) deriving (Show)

-- Default list of students as specified in the problem description.
-- Note: The problem description implies a fixed list of students,
-- but the function signature accepts a list. We'll use the provided list.
defaultStudents :: [String]
defaultStudents = ["Alice", "Bob", "Charlie", "David", "Eve", "Fred",
                   "Ginny", "Harriet", "Ileana", "Joseph", "Kincaid", "Larry"]

-- Helper function to convert a character code to a Plant.
charToPlant :: Char -> Maybe Plant
charToPlant 'C' = Just Clover
charToPlant 'G' = Just Grass
charToPlant 'R' = Just Radishes
charToPlant 'V' = Just Violets
charToPlant _   = Nothing -- Ignore invalid characters or handle as error? Let's ignore for now.

-- Helper function to parse the plant string into two rows.
parsePlants :: String -> Maybe (String, String)
parsePlants plantsStr = case lines plantsStr of
                         (r1:r2:_) -> Just (r1, r2)
                         _         -> Nothing -- Invalid format

-- Helper function to split a list into chunks of size 2.
pairs :: [a] -> [[a]]
pairs [] = []
pairs (x:y:xs) = [x, y] : pairs xs
pairs [_] = [] -- Ignore odd elements at the end, though problem implies even length.


-- | Creates a garden map from a list of students and a plant layout string.
-- | The students list provided to this function will be sorted alphabetically
-- | before assigning plants.
garden :: [String] -> String -> Garden
garden students plantsStr =
  let sortedStudents = sort students
      (row1, row2) = fromMaybe ("", "") (parsePlants plantsStr) -- Handle potential parse failure
      -- Split each row into pairs of cups for each student
      row1Pairs = pairs row1
      row2Pairs = pairs row2
      -- Combine the pairs for each student (2 from row1, 2 from row2)
      -- Ensure we don't crash if rows are shorter than expected
      studentChars = zipWith (\p1 p2 -> p1 ++ p2) row1Pairs row2Pairs
      -- Convert characters to Plants, filtering out invalid ones
      studentPlants = map (mapMaybe charToPlant) studentChars
      -- Create a map from student name to their list of plants
      plantMap = Map.fromList $ zip sortedStudents studentPlants
  in Garden plantMap

-- Helper function using mapMaybe to convert a list of Chars to [Plant]
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x:xs) = case f x of
                      Just y  -> y : mapMaybe f xs
                      Nothing -> mapMaybe f xs


-- | Looks up the plants for a given student in the garden.
-- | Returns an empty list if the student is not found.
lookupPlants :: String -> Garden -> [Plant]
lookupPlants student (Garden plantMap) =
  fromMaybe [] (Map.lookup student plantMap)

