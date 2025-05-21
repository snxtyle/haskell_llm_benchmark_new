module Garden
    ( Plant (..)
    , Garden(..) -- Expose the Garden data constructor
    , garden
    , lookupPlants
    ) where

import Data.List (lookup) -- 'lookup' is part of Data.List, which is in base

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

-- Define the Garden data type to hold the mapping from student names to their plants.
-- It wraps a list of (student name, list of plants) tuples.
data Garden = Garden [(String, [Plant])] deriving (Show)

-- Helper function to convert a single character code from the diagram to a Plant type.
charToPlant :: Char -> Plant
charToPlant 'G' = Grass
charToPlant 'C' = Clover
charToPlant 'R' = Radishes
charToPlant 'V' = Violets
charToPlant c   = error $ "Invalid plant code: " ++ [c]

-- Helper function to extract the two plants for a specific student from a single row string.
-- studentIndex is 0-based, corresponding to the student's alphabetical position.
getStudentRowPlants :: String -> Int -> [Plant]
getStudentRowPlants rowString studentIndex =
    let
        -- Each student gets 2 cups, so their plants start at index 2 * studentIndex
        startIdx = studentIndex * 2
        -- Extract the two characters for this student from the row
        char1 = rowString !! startIdx
        char2 = rowString !! (startIdx + 1)
    in
        -- Convert characters to Plant types
        [charToPlant char1, charToPlant char2]

-- The main garden function.
-- It takes a list of student names (assumed to be in alphabetical order) and the diagram string.
-- It returns a Garden structure containing each student's assigned plants.
garden :: [String] -> String -> Garden
garden students diagram =
    let
        -- Split the diagram string into two rows based on newline characters.
        rows = lines diagram
        row1 = head rows -- The first row of plants
        row2 = last rows  -- The second row of plants

        -- Generate a list of (studentName, [Plant]) tuples.
        -- We zip the student names with their 0-based index to correctly extract plants.
        studentPlantAssignments =
            [   (studentName, (getStudentRowPlants row1 idx) ++ (getStudentRowPlants row2 idx))
            |   (idx, studentName) <- zip [0..] students
            ]
    in
        -- Wrap the list of assignments in the Garden data constructor.
        Garden studentPlantAssignments

-- The lookupPlants function.
-- It takes a student's name and a Garden structure.
-- It returns the list of Plant types assigned to that student.
lookupPlants :: String -> Garden -> [Plant]
lookupPlants student (Garden gardenList) =
    -- Use Data.List.lookup to find the plants associated with the given student name.
    case Data.List.lookup student gardenList of
        Just plants -> plants -- If found, return the list of plants.
        Nothing     -> error $ "Student '" ++ student ++ "' not found in this garden." -- If not found, raise an error.
