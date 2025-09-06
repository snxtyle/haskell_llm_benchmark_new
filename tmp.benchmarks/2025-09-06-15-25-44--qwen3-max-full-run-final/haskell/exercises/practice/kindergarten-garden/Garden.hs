module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

-- Map student names to their plants
type Garden = Map String [Plant]

-- Default list of students
defaultStudents :: [String]
defaultStudents = ["Alice", "Bob", "Charlie", "David", "Eve", "Fred", "Ginny", "Harriet", "Ileana", "Joseph", "Kincaid", "Larry"]

-- Convert a character to a Plant
charToPlant :: Char -> Plant
charToPlant 'C' = Clover
charToPlant 'G' = Grass
charToPlant 'R' = Radishes
charToPlant 'V' = Violets
charToPlant _ = error "Invalid plant code"

garden :: [String] -> String -> Garden
garden studentList diagram = 
    let sortedStudents = sort studentList
        rows = lines diagram
        row1 = if length rows > 0 then rows !! 0 else ""
        row2 = if length rows > 1 then rows !! 1 else ""
        -- Group plants for each student (2 from each row)
        studentPlants = map (\i -> 
            let startIdx = i * 2
                plants1 = take 2 (drop startIdx row1)
                plants2 = take 2 (drop startIdx row2)
                allPlants = plants1 ++ plants2
            in map charToPlant allPlants) [0..length sortedStudents - 1]
    in Map.fromList (zip sortedStudents studentPlants)

lookupPlants :: String -> Garden -> [Plant]
lookupPlants student garden = 
    case Map.lookup student garden of
        Just plants -> plants
        Nothing -> []
