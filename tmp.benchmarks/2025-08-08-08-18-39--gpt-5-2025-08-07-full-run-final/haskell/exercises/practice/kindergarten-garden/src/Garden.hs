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

-- Store assignments as an association list from student name to their plants.
data Garden = Garden [(String, [Plant])]

garden :: [String] -> String -> Garden
garden students plantsDiagram =
    let sortedStudents = sort students
        ls = lines plantsDiagram
        r1 = case ls of
               (x:_)    -> x
               _        -> ""
        r2 = case ls of
               (_:y:_)  -> y
               _        -> ""
        chunks :: String -> [String]
        chunks []         = []
        chunks [_]        = [] -- ignore trailing odd cup if any (not expected in valid input)
        chunks (a:b:rest) = [a,b] : chunks rest

        row1Chunks = chunks r1
        row2Chunks = chunks r2

        charToPlant :: Char -> Plant
        charToPlant c = case c of
          'C' -> Clover
          'G' -> Grass
          'R' -> Radishes
          'V' -> Violets
          _   -> error ("Unknown plant code: " ++ [c])

        studentAssignments =
          zipWith
            (\name (c1, c2) -> (name, map charToPlant c1 ++ map charToPlant c2))
            sortedStudents
            (zip row1Chunks row2Chunks)
    in Garden studentAssignments

lookupPlants :: String -> Garden -> [Plant]
lookupPlants student (Garden assignments) =
    fromMaybe [] (lookup student assignments)
