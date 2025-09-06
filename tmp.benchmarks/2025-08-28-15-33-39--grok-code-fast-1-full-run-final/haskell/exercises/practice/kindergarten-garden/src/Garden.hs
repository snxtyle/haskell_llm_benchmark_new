module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import Data.Maybe

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

data Garden = Garden [(String, [Plant])]

charToPlant :: Char -> Plant
charToPlant 'C' = Clover
charToPlant 'G' = Grass
charToPlant 'R' = Radishes
charToPlant 'V' = Violets
charToPlant c = error $ "Invalid plant char: " ++ [c]

getPlants :: String -> Int -> Int -> [Plant]
getPlants plants rowLength i = map charToPlant [plants !! (2*i), plants !! (2*i+1), plants !! (rowLength + 2*i), plants !! (rowLength + 2*i +1)]

garden :: [String] -> String -> Garden
garden students plants = Garden $ zip students (map (getPlants plants rowLength) [0..length students - 1])
  where rowLength = 2 * length students

lookupPlants :: String -> Garden -> [Plant]
lookupPlants student (Garden g) = fromJust $ lookup student g
