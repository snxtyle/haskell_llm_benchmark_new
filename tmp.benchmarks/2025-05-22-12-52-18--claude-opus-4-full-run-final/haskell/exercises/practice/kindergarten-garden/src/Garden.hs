module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import Data.List (sort, elemIndex)
import Data.Maybe (fromMaybe)

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

data Garden = Garden { plantRows :: [String]
                     , studentList :: [String]
                     } deriving (Show)

-- Default student list in alphabetical order
defaultStudents :: [String]
defaultStudents = ["Alice", "Bob", "Charlie", "David", "Eve", "Fred",
                   "Ginny", "Harriet", "Ileana", "Joseph", "Kincaid", "Larry"]

-- Convert a character to a Plant
charToPlant :: Char -> Plant
charToPlant 'C' = Clover
charToPlant 'G' = Grass
charToPlant 'R' = Radishes
charToPlant 'V' = Violets
charToPlant _   = error "Invalid plant character"

-- Create a garden from student list and plant diagram
garden :: [String] -> String -> Garden
garden students plants = Garden { plantRows = lines plants
                                , studentList = sort students
                                }

-- Look up plants for a specific student
lookupPlants :: String -> Garden -> [Plant]
lookupPlants student (Garden rows students) =
    case elemIndex student students of
        Nothing -> []
        Just idx -> 
            let startPos = idx * 2
                row1 = head rows
                row2 = rows !! 1
                -- Get 2 plants from each row for this student
                plantsRow1 = take 2 $ drop startPos row1
                plantsRow2 = take 2 $ drop startPos row2
            in map charToPlant (plantsRow1 ++ plantsRow2)
