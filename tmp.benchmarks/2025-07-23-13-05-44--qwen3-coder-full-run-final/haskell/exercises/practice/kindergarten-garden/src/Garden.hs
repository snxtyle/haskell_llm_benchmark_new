module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import Data.List (sort)

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

data Garden = Garden [(String, [Plant])]
            deriving (Show)

-- Convert a character to a Plant
charToPlant :: Char -> Plant
charToPlant 'C' = Clover
charToPlant 'G' = Grass
charToPlant 'R' = Radishes
charToPlant 'V' = Violets
charToPlant _   = error "Invalid plant character"

-- Parse the garden diagram and create a Garden
garden :: [String] -> String -> Garden
garden students plants = 
    let sortedStudents = sort students
        [row1, row2] = lines plants
        plantRows = [map charToPlant row1, map charToPlant row2]
        studentPlants = zip sortedStudents (assignPlants plantRows)
    in Garden studentPlants
  where
    assignPlants [row1Plants, row2Plants] = 
        let pairs1 = groupPairs row1Plants
            pairs2 = groupPairs row2Plants
        in zipWith (++) pairs1 pairs2
    assignPlants _ = error "Exactly two rows expected"
    
    groupPairs [] = []
    groupPairs (a:b:rest) = [a,b] : groupPairs rest
    groupPairs _ = error "Odd number of plants"

-- Look up the plants for a specific student
lookupPlants :: String -> Garden -> [Plant]
lookupPlants student (Garden studentPlants) = 
    case lookup student studentPlants of
        Just plants -> plants
        Nothing -> []
