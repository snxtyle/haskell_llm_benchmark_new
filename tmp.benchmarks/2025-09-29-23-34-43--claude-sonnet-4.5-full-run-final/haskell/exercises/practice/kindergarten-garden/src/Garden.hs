module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import Data.List (sort)
import Data.Maybe (fromMaybe)

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

data Garden = Garden [(String, [Plant])]

defaultStudents :: [String]
defaultStudents = ["Alice", "Bob", "Charlie", "David", "Eve", "Fred",
                   "Ginny", "Harriet", "Ileana", "Joseph", "Kincaid", "Larry"]

charToPlant :: Char -> Plant
charToPlant 'G' = Grass
charToPlant 'C' = Clover
charToPlant 'R' = Radishes
charToPlant 'V' = Violets
charToPlant _   = error "Invalid plant character"

garden :: [String] -> String -> Garden
garden students plants = Garden studentPlants
  where
    sortedStudents = sort students
    rows = lines plants
    row1 = if null rows then "" else head rows
    row2 = if length rows < 2 then "" else rows !! 1
    
    studentPlants = zipWith assignPlants sortedStudents [0..]
    
    assignPlants :: String -> Int -> (String, [Plant])
    assignPlants student idx =
      let startCol = idx * 2
          plant1 = if startCol < length row1 then charToPlant (row1 !! startCol) else Grass
          plant2 = if startCol + 1 < length row1 then charToPlant (row1 !! (startCol + 1)) else Grass
          plant3 = if startCol < length row2 then charToPlant (row2 !! startCol) else Grass
          plant4 = if startCol + 1 < length row2 then charToPlant (row2 !! (startCol + 1)) else Grass
      in (student, [plant1, plant2, plant3, plant4])

lookupPlants :: String -> Garden -> [Plant]
lookupPlants student (Garden studentPlants) =
  fromMaybe [] (lookup student studentPlants)
