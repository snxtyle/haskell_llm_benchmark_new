module Garden
    ( Plant (..)
    , Garden
    , garden
    , lookupPlants
    ) where

import Data.List (elemIndex, sort)

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

-- | A Garden represents the layout of plants and the students assigned to them.
data Garden = Garden { students :: [String]
                     , firstRow :: String
                     , secondRow :: String
                     } deriving (Show)

-- | Creates a Garden from a list of student names and a diagram string.
-- The students are sorted alphabetically to determine their plot assignments.
-- The diagram string contains two rows of plant codes, separated by a newline.
garden :: [String] -> String -> Garden
garden students plants =
    let sortedStudents = sort students
        [row1, row2] = lines plants
    in Garden { students = sortedStudents, firstRow = row1, secondRow = row2 }

-- | Looks up the plants for a given student in a Garden.
-- Returns a list of four plants, representing the student's two cups in each row.
lookupPlants :: String -> Garden -> [Plant]
lookupPlants student gardenData =
    case elemIndex student (students gardenData) of
        Nothing -> [] -- Or error "Student not found in garden"
        Just index ->
            let startPos = index * 2
                cupsPerRow = 2
                firstRowCups = take cupsPerRow $ drop startPos $ firstRow gardenData
                secondRowCups = take cupsPerRow $ drop startPos $ secondRow gardenData
                allCups = firstRowCups ++ secondRowCups
            in map charToPlant allCups

-- | Helper function to convert a character from the diagram to a Plant type.
charToPlant :: Char -> Plant
charToPlant 'C' = Clover
charToPlant 'G' = Grass
charToPlant 'R' = Radishes
charToPlant 'V' = Violets
charToPlant c   = error $ "Invalid plant character: " ++ [c]
