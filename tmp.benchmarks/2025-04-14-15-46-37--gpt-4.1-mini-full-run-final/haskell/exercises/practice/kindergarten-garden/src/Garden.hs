module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

-- Garden holds a map from student name to their plants
newtype Garden = Garden (Map String [Plant])

-- The 12 children in alphabetical order
students :: [String]
students = ["Alice", "Bob", "Charlie", "David", "Eve", "Fred", "Ginny",
            "Harriet", "Ileana", "Joseph", "Kincaid", "Larry"]

-- Convert a character to a Plant
charToPlant :: Char -> Plant
charToPlant 'G' = Grass
charToPlant 'C' = Clover
charToPlant 'R' = Radishes
charToPlant 'V' = Violets
charToPlant _   = error "Invalid plant character"

garden :: [String] -> String -> Garden
garden studentsList plantsStr =
    let
        -- Sort students alphabetically
        sortedStudents = sort studentsList

        -- The garden diagram has two rows
        (row1, row2) = splitAt (length plantsStr `div` 2) plantsStr

        -- Each student gets 2 plants per row, so 4 plants total
        -- Group plants in pairs for each row
        pairsRow1 = chunksOf 2 row1
        pairsRow2 = chunksOf 2 row2

        -- Combine pairs from both rows for each student
        studentPlants = zipWith (\p1 p2 -> map charToPlant (p1 ++ p2)) pairsRow1 pairsRow2

        -- Map from student to their plants
        gardenMap = Map.fromList $ zip sortedStudents studentPlants
    in Garden gardenMap

lookupPlants :: String -> Garden -> [Plant]
lookupPlants student (Garden g) =
    Map.findWithDefault [] student g

-- Helper function to split a list into chunks of n elements
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t
