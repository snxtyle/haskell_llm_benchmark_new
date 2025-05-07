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

-- The Garden type now holds a map from student names to their assigned plants.
newtype Garden = Garden (Map String [Plant])
    deriving (Eq, Show)

-- Helper function to convert a character representation to a Plant.
charToPlant :: Char -> Plant
charToPlant 'C' = Clover
charToPlant 'G' = Grass
charToPlant 'R' = Radishes
charToPlant 'V' = Violets
charToPlant  c  = error ("Invalid plant character: " ++ [c])

-- garden function processes the student list and plant diagram to create a Garden.
garden :: [String] -> String -> Garden
garden studentNames plantsDiagram =
    let rows = lines plantsDiagram
    in if length rows /= 2
       then error "Plant diagram must have exactly two rows."
       else let [row1_str, row2_str] = rows
                sortedStudents = sort studentNames
                
                plantAssignments = map assignPlantsForStudent (zip [0..] sortedStudents)
                  where
                    assignPlantsForStudent (idx, studentName) =
                        if (2*idx + 1 >= length row1_str) || (2*idx + 1 >= length row2_str)
                        then error ("Plant diagram rows are too short for student: " ++ studentName)
                        else let plantChars = [ row1_str !! (2*idx)
                                             , row1_str !! (2*idx+1)
                                             , row2_str !! (2*idx)
                                             , row2_str !! (2*idx+1)
                                             ]
                                studentPlants = map charToPlant plantChars
                            in (studentName, studentPlants)
            in Garden (Map.fromList plantAssignments)

-- lookupPlants retrieves the list of plants for a given student from the Garden.
lookupPlants :: String -> Garden -> [Plant]
lookupPlants studentName (Garden plantMap) =
    case Map.lookup studentName plantMap of
        Just plants -> plants
        Nothing     -> error ("Student " ++ studentName ++ " not found in garden.")
