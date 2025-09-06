module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import Data.List (elemIndex, sort)

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

data Garden = Garden [String] String String

charToPlant :: Char -> Plant
charToPlant 'C' = Clover
charToPlant 'G' = Grass
charToPlant 'R' = Radishes
charToPlant 'V' = Violets
charToPlant _   = error "Invalid plant character"

garden :: [String] -> String -> Garden
garden students plants = Garden (sort students) firstRow secondRow
  where
    rows = lines plants
    firstRow = head rows
    secondRow = rows !! 1

lookupPlants :: String -> Garden -> [Plant]
lookupPlants student (Garden students row1 row2) = 
    case elemIndex student students of
        Just idx -> 
            let start = idx * 2
                plants1 = take 2 (drop start row1)
                plants2 = take 2 (drop start row2)
            in map charToPlant (plants1 ++ plants2)
        Nothing  -> error "Student not found in the garden"
