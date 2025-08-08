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

data Garden = Garden
    { gardenStudents :: [String]
    , gardenRows :: ([Plant], [Plant])
    }

-- Default students if none provided
defaultStudents :: [String]
defaultStudents = 
    [ "Alice", "Bob", "Charlie", "David"
    , "Eve", "Fred", "Ginny", "Harriet"
    , "Ileana", "Joseph", "Kincaid", "Larry"
    ]

-- Convert a character to a Plant
charToPlant :: Char -> Plant
charToPlant 'C' = Clover
charToPlant 'G' = Grass
charToPlant 'R' = Radishes
charToPlant 'V' = Violets
charToPlant _   = error "Invalid plant character"

-- Parse a row string into a list of Plants
parseRow :: String -> [Plant]
parseRow = map charToPlant

garden :: [String] -> String -> Garden
garden students plants = 
    let rows = lines plants
        sortedStudents = if null students 
                        then defaultStudents 
                        else sort students
        (row1, row2) = case rows of
            [r1, r2] -> (parseRow r1, parseRow r2)
            _        -> error "Garden must have exactly 2 rows"
    in Garden sortedStudents (row1, row2)

lookupPlants :: String -> Garden -> [Plant]
lookupPlants student (Garden students (row1, row2)) =
    let maybeIndex = elemIndex student students
        index = fromMaybe (error "Student not found") maybeIndex
        -- Each student gets 2 consecutive plants from each row
        startPos = index * 2
        plants1 = take 2 $ drop startPos row1
        plants2 = take 2 $ drop startPos row2
    in plants1 ++ plants2
