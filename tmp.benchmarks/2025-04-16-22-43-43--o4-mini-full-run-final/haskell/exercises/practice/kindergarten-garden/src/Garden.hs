module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import Data.List  (sort, elemIndex)
import Data.Maybe (fromJust)

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

-- Garden holds the sorted list of students and the two rows of plant codes
data Garden = Garden [String] [String]

-- Create a Garden from a list of student names and the diagram string.
garden :: [String] -> String -> Garden
garden students plants =
    let sortedStudents = sort students
        rows = lines plants
    in Garden sortedStudents rows

-- Given a student's name and a Garden, return their four plants.
lookupPlants :: String -> Garden -> [Plant]
lookupPlants student (Garden students [row1, row2]) =
    let idx = fromJust (elemIndex student students)
        -- Compute the two positions in each row
        positions = [2*idx, 2*idx + 1]
        chars  = [ row1 !! p | p <- positions ] ++ [ row2 !! p | p <- positions ]
    in map charToPlant chars
lookupPlants _ _ = []  -- Fallback if the garden is malformed

-- Map a diagram character to a Plant
charToPlant :: Char -> Plant
charToPlant 'C' = Clover
charToPlant 'G' = Grass
charToPlant 'R' = Radishes
charToPlant 'V' = Violets
charToPlant  c  = error $ "Invalid plant character: " ++ [c]
