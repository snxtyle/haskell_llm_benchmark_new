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
    , gardenRow1 :: [Plant]
    , gardenRow2 :: [Plant]
    } deriving (Show)

garden :: [String] -> String -> Garden
garden students plants = Garden sortedStudents row1Plants row2Plants
  where
    sortedStudents = sort students
    plantLines = lines plants
    row1Plants = map charToPlant (head plantLines)
    row2Plants = map charToPlant (plantLines !! 1)

charToPlant :: Char -> Plant
charToPlant 'G' = Grass
charToPlant 'C' = Clover
charToPlant 'R' = Radishes
charToPlant 'V' = Violets
charToPlant _ = error "Invalid plant character"

lookupPlants :: String -> Garden -> [Plant]
lookupPlants student (Garden students row1 row2) = 
    case elemIndex student students of
        Nothing -> []
        Just index -> 
            let startPos = index * 2
                plant1 = row1 !! startPos
                plant2 = row1 !! (startPos + 1)
                plant3 = row2 !! startPos
                plant4 = row2 !! (startPos + 1)
            in [plant1, plant2, plant3, plant4]
