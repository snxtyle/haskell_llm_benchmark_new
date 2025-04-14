module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

data Garden = Garden [(String, [Plant])]

-- Default list of students in alphabetical order
defaultStudents :: [String]
defaultStudents = ["Alice", "Bob", "Charlie", "David", "Eve", "Fred", "Ginny", "Harriet", "Ileana", "Joseph", "Kincaid", "Larry"]

-- Convert a character to a Plant
charToPlant :: Char -> Plant
charToPlant 'G' = Grass
charToPlant 'C' = Clover
charToPlant 'R' = Radishes
charToPlant 'V' = Violets
charToPlant _ = error "Invalid plant character"

-- Create a garden from a list of students and a string representing the plant diagram
garden :: [String] -> String -> Garden
garden students plants = Garden $ zip students' plantsByStudent
  where
    -- Use provided students or default if empty
    students' = if null students then defaultStudents else students
    -- Split the plant diagram into rows
    rows = lines plants
    -- Convert each row into a list of Plants
    plantRows = map (map charToPlant) rows
    -- Group plants by student (each student gets 2 plants per row)
    plantsByStudent = map (\i -> concatMap (\row -> take 2 $ drop (i * 2) row) plantRows) [0..length students' - 1]

-- Lookup the plants for a specific student
lookupPlants :: String -> Garden -> [Plant]
lookupPlants student (Garden assignments) = 
    case lookup student assignments of
        Just plants -> plants
        Nothing -> []
