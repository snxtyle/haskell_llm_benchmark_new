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

-- Represent the garden as an association list of students to their plants.
data Garden = Garden [(String, [Plant])]

charToPlant :: Char -> Plant
charToPlant 'C' = Clover
charToPlant 'G' = Grass
charToPlant 'R' = Radishes
charToPlant 'V' = Violets
charToPlant c   = error $ "Invalid plant code: " ++ [c]

divideRow :: String -> [String]
divideRow [] = []
divideRow (a:b:rest) = [a,b] : divideRow rest
divideRow _ = error "Row must have even length"  -- This should not happen if we've ensured even length.

garden :: [String] -> String -> Garden
garden students diagram = 
    let sortedStudents = sort students
        n = length sortedStudents
    in case lines diagram of
        (r1:r2:_) -> 
            if length r1 >= 2 * n && length r2 >= 2 * n
                then let row1 = take (2*n) r1
                         row2 = take (2*n) r2
                         chunks1 = divideRow row1
                         chunks2 = divideRow row2
                     in Garden $ zipWith (\i student -> 
                             (student, 
                              map charToPlant (chunks1 !! i) ++ map charToPlant (chunks2 !! i))
                         ) [0..] sortedStudents
                else error "Each row must have at least 2*(number of children) plants"
        _ -> error "Garden diagram must have at least two rows"

lookupPlants :: String -> Garden -> [Plant]
lookupPlants student (Garden assoc) = 
    case lookup student assoc of
        Just plants -> plants
        Nothing -> error $ "Student not found: " ++ student
